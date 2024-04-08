{-# LANGUAGE OverloadedStrings #-}

module Primitive (primEnv) where 

import CrispVal (
        CrispException(NumOfArgs, InvalidArgument),
        FunWrapper(FunWrapper),
        CrispVal(Atom, Fun, Number, String, Bool, Nil, List),
        Eval
    )

import Data.Text as T ( Text, concat, pack, unpack, unwords )
import Control.Monad.Except (foldM, MonadIO(liftIO))
import Control.Exception (throw)

type PrimtiveFunctions = [(T.Text, CrispVal)]
type UnaryFunction = CrispVal -> Eval CrispVal
type BinaryFunction = CrispVal -> CrispVal -> Eval CrispVal

-- Wrap function into CrispVal
buildFunction :: ([CrispVal] -> Eval CrispVal) -> CrispVal
buildFunction = Fun . FunWrapper

-- apply unary function 
applyUnary :: UnaryFunction -> [CrispVal] -> Eval CrispVal
applyUnary f [x] = f x 
applyUnary _ args = throw $ NumOfArgs 1 args 

-- apply binary function 
applyBinary :: BinaryFunction -> [CrispVal] -> Eval CrispVal
applyBinary f [x, y] = f x y 
applyBinary _ args = throw $ NumOfArgs 2 args

-- fold a binary function over more than 2 arguments
foldBinary :: BinaryFunction -> CrispVal -> [CrispVal] -> Eval CrispVal
foldBinary f acc [] = throw $ NumOfArgs 2 [] 
foldBinary f acc args = foldM f acc args

-- primitive environment 
primEnv :: PrimtiveFunctions
primEnv = [
        ("+", buildFunction $ foldBinary (applyBinaryNumeric (+)) (Number 0)),
        ("*", buildFunction $ foldBinary (applyBinaryNumeric (*)) (Number 1)),
        ("++", buildFunction $ foldBinary (applyBinaryString (<>)) (String "")),
        ("-", buildFunction $ applyBinary $ applyBinaryNumeric (-)),
        ("<", buildFunction $ applyBinary $ applyNumericComparator (<)),
        ("<=", buildFunction $ applyBinary $ applyNumericComparator (<=)),
        (">", buildFunction $ applyBinary $ applyNumericComparator (>)),
        (">=", buildFunction $ applyBinary $ applyNumericComparator (>=)),
        ("==", buildFunction $ applyBinary $ applyNumericComparator (==)),
        ("&&", buildFunction $ foldBinary (applyBoolean (&&)) (Bool True)),
        ("||", buildFunction $ foldBinary (applyBoolean (||)) (Bool False)),
        ("!", buildFunction $ applyUnary applyNegation),
        ("construct", buildFunction construct),
        ("head", buildFunction Primitive.head),
        ("tail", buildFunction Primitive.tail)   
    ]

-- apply numeric function
applyBinaryNumeric :: (Integer -> Integer -> Integer) -> CrispVal -> CrispVal -> Eval CrispVal
applyBinaryNumeric f (Number x) (Number y) = return . Number $ f x y
applyBinaryNumeric _ (Number _) arg2 = throw $ InvalidArgument ["number"] [arg2] 
applyBinaryNumeric _ arg1 (Number _) = throw $ InvalidArgument ["number"] [arg1] 
applyBinaryNumeric _ arg1 arg2 = throw $ InvalidArgument ["number", "number"] [arg1, arg2]

-- apply a boolean function to a number
applyBooleanToNumber :: (Integer -> Bool) -> CrispVal -> Eval CrispVal
applyBooleanToNumber f (Number x) = return . Bool $ f x
applyBooleanToNumber _ arg = throw $ InvalidArgument ["number"] [arg]

-- apply a binary string function
applyBinaryString :: (T.Text -> T.Text -> T.Text) -> CrispVal -> CrispVal -> Eval CrispVal
applyBinaryString f (String x) (String y) = return . String $ f x y
applyBinaryString _ (String _) arg2 = throw $ InvalidArgument ["string"] [arg2]
applyBinaryString _ arg1 (String _) = throw $ InvalidArgument ["string"] [arg1]
applyBinaryString _ arg1 arg2 = throw $ InvalidArgument ["string", "string"] [arg1, arg2]

-- apply a boolean operator
applyBoolean :: (Bool -> Bool -> Bool) -> CrispVal -> CrispVal -> Eval CrispVal
applyBoolean f (Bool x) (Bool y) = return . Bool $ f x y 
applyBoolean _ (Bool _) arg2 = throw $ InvalidArgument ["boolean"] [arg2]
applyBoolean _ arg1 (Bool _) = throw $ InvalidArgument ["boolean"] [arg1]
applyBoolean _ arg1 arg2 = throw $ InvalidArgument ["boolean", "boolean"] [arg1, arg2]

-- apply a comparator to numbers 
applyNumericComparator :: (Integer -> Integer -> Bool) -> CrispVal -> CrispVal -> Eval CrispVal
applyNumericComparator f (Number x) (Number y) = return . Bool $ f x y 
applyNumericComparator _ (Number _) arg2 = throw $ InvalidArgument ["number"] [arg2]
applyNumericComparator _ arg1 (Number _) = throw $ InvalidArgument ["number"] [arg1]
applyNumericComparator _ arg1 arg2 = throw $ InvalidArgument ["number", "number"] [arg1, arg2]

-- apply boolean not
applyNegation :: CrispVal -> Eval CrispVal 
applyNegation (Bool True) = return $ Bool False
applyNegation (Bool False) = return $ Bool True
applyNegation arg = throw $ InvalidArgument ["boolean"] [arg]

-- functions for list manipulation
-- construct list
construct :: [CrispVal] -> Eval CrispVal
construct [x, List y] = return . List $ x:y
construct [x, y] = return . List $ [x, y]
construct arg = throw $ InvalidArgument ["list"] [List arg]

-- returns the head of the list
head :: [CrispVal] -> Eval CrispVal
head [List []] = return Nil
head [List (x:_)] = return x
head arg = throw $ InvalidArgument ["list"] [List arg]

-- returns the tail of the list
tail :: [CrispVal] -> Eval CrispVal
tail [List []] = return Nil
tail [List (_:xs)] = return . List $ xs
tail arg = throw $ InvalidArgument ["list"] [List arg]

-- quote :: [CrispVal] -> Eval CrispVal
-- quote [List xs] = return . List . Atom $ "quote" : xs
-- quote [expr]

