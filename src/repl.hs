{-# LANGUAGE OverloadedStrings #-}

module REPL (
    replLoop,
) where

import Eval (safeExec, evalText)

import Control.Monad.Trans (MonadIO (liftIO))
import System.Console.Haskeline
    (defaultSettings, getInputLine, outputStrLn, runInputT, InputT)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void 
import Data.Text as T
import Data.Functor
import qualified Text.Megaparsec.Char.Lexer as L
import System.Exit (exitSuccess)
import Parser (readCode)

-- REPL Parser

type Parser = Parsec Void Text

-- Consumes whitespace and comments
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "//") (L.skipBlockCommentNested "/*" "*/")

-- Automatically consume trailing spaces after a lexeme
lexeme = L.lexeme spaceConsumer

-- Options provided: ":ast" ":quit"
data Option = Quit | Run T.Text
            deriving (Show)

quit :: Parser Option
quit = (try $ string ":quit"
        <|> string ":q") $> Quit

-- run expr
runExpr :: Parser Option
runExpr = try $ do
    expr <- some asciiChar
    return . Run . T.pack $ expr

-- parse options
options :: Parser Option
options = quit
        <|> runExpr

contents :: Parser a -> Parser a
contents x = spaceConsumer *> lexeme x <* eof

readREPLLine :: T.Text -> Either (ParseErrorBundle T.Text Void) Option
readREPLLine = parse (contents options) "<stdin>"

-- REPL
type REPL a = InputT IO a

process :: String -> IO ()
process s = do
    let line = readREPLLine . T.pack $ s
    case line of 
        Left _ -> putStrLn "Usage: [options] expression\nwhere options include:\n\t:quit\t\tquit REPL"
        Right opt -> 
            case opt of 
                Quit -> exitSuccess
                Run expr -> do
                    res <- safeExec . evalText $ expr
                    either putStrLn return res

repl :: REPL ()
repl = do
    input <- getInputLine "crisp> "
    case input of 
        Nothing -> outputStrLn "Sayonara"
        Just x -> liftIO (process x) >> repl

replLoop :: IO ()
replLoop = runInputT defaultSettings repl



