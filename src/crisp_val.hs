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

-- Environment in which evaluation happens
type Env = Map.Map T.Text CrispVal

-- Evaluation monad transformer with Reader (Env) and IO
newtype Eval a = Eval { getEval :: ReaderT Env IO a}
                deriving (Monad, Functor, Applicative, MonadReader Env, MonadIO)

-- Wrapper to represent a function with args as a list of CrispVal 
newtype FunWrapper = FunWrapper { getFun :: [CrispVal] -> Eval CrispVal}

instance Eq FunWrapper where 
    (==) :: FunWrapper -> FunWrapper -> Bool
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

-- Printing exception
showException :: CrispException -> T.Text
showException exp = case exp of 
                        (NumOfArgs n args) -> "exception"

instance Exception CrispException

instance Show CrispException where 
    show :: CrispException -> String
    show = T.unpack . showException 







