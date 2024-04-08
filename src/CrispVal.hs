{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module CrispVal (
    CrispVal(..),
    Eval(..),
    FunWrapper(..),
    Env,
    CrispException(..)
) where 

import Data.Text as T
import qualified Data.Map as Map 
import Control.Monad.Reader
import Control.Exception

-- Env for variables and functions
type VarEnv = Map.Map T.Text CrispVal 
type FuncEnv = Map.Map T.Text CrispVal

-- Environment in which evaluation happens
data Env = Env {
    vEnv :: VarEnv,
    fEnv :: FuncEnv
} deriving (Eq)

-- Evaluation monad transformer with Reader (Env) and IO
newtype Eval a = Eval { getEval :: ReaderT Env IO a}
                deriving (Monad, Functor, Applicative, MonadReader Env, MonadIO)

-- Wrapper to represent a function with args as a list of CrispVal 
newtype FunWrapper = FunWrapper { getFun :: [CrispVal] -> Eval CrispVal}

instance Eq FunWrapper where 
    (==) _ _ = False 

-- Used in the internal representation of the code
data CrispVal = Atom T.Text         -- Represents variables (or keywords)
            | List [CrispVal]       -- S-expr
            | Number Integer 
            | String T.Text 
            | Nil
            | Fun FunWrapper        -- operators
            | Lambda FunWrapper Env 
            | Bool Bool 
            deriving (Eq)  

-- Printing CrispVal
toString :: CrispVal -> T.Text
toString val = case val of 
                    (Atom v) -> v 
                    (List xs) -> T.concat ["(", toStringList xs, ")"]
                    (Number n) -> T.pack $ show n
                    (String s) -> T.concat ["\"", s, "\""]
                    Nil -> "Nil"
                    Fun _ -> "(internal function)"
                    Lambda _ _ -> "(lambda function)"
                    (Bool b) -> T.pack $ show b


toStringList :: [CrispVal] -> T.Text 
toStringList = T.unwords . Prelude.map toString

instance Show CrispVal where 
    show = T.unpack . toString

-- For exceptions
data CrispException = NumOfArgs Integer [CrispVal]
                    | InvalidArgument [T.Text] [CrispVal]


-- Printing exception
showException :: CrispException -> T.Text
showException exp = case exp of 
                        (NumOfArgs n args) -> T.concat ["Invalid number of arguments, expected: ", T.pack $ show n, " found: ", T.pack . show . Prelude.length $ args]
                        (InvalidArgument expected args) -> T.concat ["Invalid argument, expected: ", T.unwords expected, " found: ", T.unwords $ toString <$> args]
instance Exception CrispException

instance Show CrispException where 
    show = T.unpack . showException 







