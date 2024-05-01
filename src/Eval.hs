{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval (
    evalText,
    evalFile,
    safeExec,
    runParseTest
) where

import Primitive (primEnv, applyUnary)
import Parser (readCode, readCodeFromFile)
import CrispVal (
        CrispException (..),
        FunWrapper (FunWrapper),
        CrispVal (..),
        Eval (getEval),
        Env (..),
        toString
    )

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Directory (doesFileExist)

import Control.Monad.Reader (
        asks, 
        MonadIO (liftIO),
        MonadReader (local, ask),
        ReaderT (runReaderT)
    )

import Control.Exception (
        try,
        throw,
        Exception (fromException),
        SomeException
    )

-- function to read in a string (from code)
readFunc :: CrispVal -> Eval CrispVal
readFunc (String s) = lineToEval s
readFunc val = throw $ InvalidArgument ["string"] [val]

-- function to parse an expr (from code)
parseFunc :: CrispVal -> Eval CrispVal
parseFunc (String s) = either (throw . ParseError . show) return $ readCode s
parseFunc val = throw $ InvalidArgument ["string"] [val]

-- enviroment consisting of bound functions
funcEnv :: Map.Map T.Text CrispVal
funcEnv = Map.fromList $ primEnv <> [
    ("read", Fun . FunWrapper $ applyUnary readFunc),
    ("parse", Fun . FunWrapper $ applyUnary parseFunc),
    ("eval", Fun . FunWrapper $ applyUnary eval),
    ("show", Fun . FunWrapper $ applyUnary (return . String . toString))]

-- whole enviroment
basicEnv :: Env
basicEnv = Env Map.empty funcEnv

-- for REPL so that it doesn't stop on exceptions
safeExec :: IO a -> IO (Either String a)
safeExec m = 
    do 
        res <- Control.Exception.try m 
        case res of 
            Left (exp :: SomeException) ->
                case fromException exp of 
                    Just (enclosed :: CrispException) -> return $ Left (show enclosed)
                    Nothing -> return $ Left (show exp)
            Right val -> return $ Right val

-- run the code in the given environment
runInEnv :: Env -> Eval a -> IO a
runInEnv env code = runReaderT (getEval code) env

-- convert string line to be evaluated
lineToEval :: T.Text -> Eval CrispVal
lineToEval s = either (throw . ParseError . show) eval $ readCode s

-- evaluate from given filename and contents
-- and print the result
evalFile :: FilePath -> T.Text -> IO ()
evalFile path contents = runInEnv basicEnv (fileToEval path contents) >>= print

-- convert file contents to be evaluated 
fileToEval :: FilePath -> T.Text -> Eval CrispVal
fileToEval path contents = 
    do 
        let code = readCodeFromFile path contents
        -- liftIO $ print code
        either (throw . ParseError . show) evalBody code

-- parse and return the AST as a String
runParseTest :: T.Text -> T.Text
runParseTest expr = either (T.pack . show) (T.pack . show) $ readCode expr

-- convert text to be evaluated
textToEval :: T.Text -> Eval CrispVal
textToEval expr = either (throw . ParseError . show) evalBody $ readCode expr

-- evaluate from text (for REPL)
evalText :: T.Text -> IO ()
evalText expr = 
    do 
        res <- runInEnv basicEnv $ textToEval expr
        print res

-- fetch the bound value of the given variable in the current env
fetchVar :: CrispVal -> Eval CrispVal
fetchVar (Atom var) = 
    do 
        Env {..} <- ask
        -- lookup in the union of both var and func env 
        -- with preference to func env
        case Map.lookup var (Map.union fEnv vEnv) of 
            Just x -> return x
            Nothing -> throw $ UnboundVar var

fetchVar x = throw $ InvalidArgument ["identifier"] [x]

-- ensure that the argument is an Atom
ensureAtom :: CrispVal -> Eval CrispVal
ensureAtom x@(Atom _) = return x
ensureAtom x = throw $ InvalidArgument ["atom"] [x]

-- extract variable name from Atom
extractVar :: CrispVal -> T.Text
extractVar (Atom x) = x
extractVar x = throw $ InvalidArgument ["atom"] [x]

-- fetch even elements
fetchEven :: [a] -> [a]
fetchEven [] = []
fetchEven (x:xs) = x : fetchOdd xs

-- fetch odd elements 
fetchOdd :: [a] -> [a]
fetchOdd [] = []
fetchOdd (_:xs) = fetchEven xs

-- apply lambda
applyLambda :: CrispVal -> [CrispVal] -> [CrispVal] -> Eval CrispVal
applyLambda expr params args = bindAndEval params args expr

-- bind the params to args and eval expr
bindAndEval :: [CrispVal] -> [CrispVal] -> CrispVal -> Eval CrispVal
bindAndEval params args expr = 
    do 
        Env {..} <- ask
        -- key value pairs
        let vars = zipWith (\a b -> (extractVar a, b)) params args
        -- split the map between functions (lambda) and vars
        let (newVEnv, newFEnv) = Map.partition (not . isLambda) $ Map.fromList vars
        -- evaluate expr in new env 
        local (const $ Env (newVEnv <> vEnv) (newFEnv <> fEnv)) $ eval expr 

-- returns true if the given argument is 
-- a lambda 
isLambda :: CrispVal -> Bool
isLambda (List ((Atom "lambda") : _)) = True
isLambda _ = False 

-- dump current environment
eval :: CrispVal -> Eval CrispVal
eval (List [Atom "dumpEnv", x]) = 
    do 
        Env {..} <- ask
        liftIO . print . Map.toList $ vEnv
        liftIO . print . Map.toList $ fEnv
        eval x

-- basic exprs
eval (Number x) = return $ Number x
eval (String s) = return $ String s
eval (Bool b) = return $ Bool b
eval (List []) = return Nil
eval Nil = return Nil
eval x@(Atom _) = fetchVar x

-- quoted expr
eval (List [Atom "quote", val]) = return val

-- if statement
eval (List [Atom "if", predicate, trueExpr, falseExpr]) = 
    do 
        res <- eval predicate
        case res of 
            (Bool True) -> eval trueExpr
            (Bool False) -> eval falseExpr
            _ -> throw $ Default "if's predicate must evaluate to bool"

eval (List ((Atom "if") : _)) = throw $ Default "Invalid syntax for if statement, expected: if <predicate> <expr> <expr>"

-- begin statement
eval (List [Atom "begin", expr]) = evalBody expr
eval (List ((Atom "begin") : xs)) = evalBody $ List xs

-- define statement (useless?)
eval (List [Atom "define", var, expr]) = 
    do 
        Env {} <- ask
        _ <- ensureAtom var 
        _ <- eval expr
        bindAndEval [var] [expr] var

-- let expr 
eval (List [Atom "let", List keyValPairs, expr]) = 
    do 
        Env{} <- ask
        vars <- mapM ensureAtom $ fetchEven keyValPairs
        vals <- mapM eval $ fetchOdd keyValPairs
        bindAndEval vars vals expr 

eval (List ((Atom "let") : _)) = throw $ Default "Invalid syntax for let expr, expected: \n\t(let (<key_value_pairs>) <expr>)"

-- lambda
eval (List [Atom "lambda", List params, expr]) = 
    do 
        asks (Lambda (FunWrapper $ applyLambda expr params))
eval (List ((Atom "lambda") : _)) = throw $ Default "Invalid syntax for lambda function, expected: \n\t(lambda <params> <expr>)"

eval (List (x : xs)) = 
    do
        -- liftIO $ print val
        Env{..} <- ask
        fun <- eval x
        args <- mapM eval xs
        case fun of 
            (Fun (FunWrapper f)) -> f args
            (Lambda (FunWrapper f) (Env boundVEnv _))
                -> local (const $ Env boundVEnv fEnv) $ f args 
            _ -> throw . Default . T.pack $ "Expected function, found: " <> show fun <> " " <> show (x:xs)
    
eval _ = throw $ Default "Fall through"

-- returns updated environment
updateEnv :: T.Text -> CrispVal -> Env -> Env
updateEnv var val@(Fun _) Env{..} = Env vEnv $ Map.insert var val fEnv
updateEnv var val@(Lambda _ _) Env{..} = Env vEnv $ Map.insert var val fEnv
updateEnv var val Env{..} = Env (Map.insert var val vEnv) fEnv

-- define statements
evalBody :: CrispVal -> Eval CrispVal
evalBody (List [List ((:) (Atom "define") [Atom var, expr]), rest]) = 
    do 
        val <- eval expr
        env <- ask
        local (const $ updateEnv var val env) $ eval rest

evalBody (List ((:) (List ((:) (Atom "define") [Atom var, expr])) rest)) =
    do 
        val <- eval expr
        env <- ask
        local (const $ updateEnv var val env) $ evalBody $ List rest
    
evalBody (List [x]) = eval x
evalBody (List (x : xs)) =
    do 
        _ <- eval x
        evalBody . List $ xs

evalBody x = eval x