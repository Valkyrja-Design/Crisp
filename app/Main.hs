{-# LANGUAGE OverloadedStrings #-}

import REPL (replLoop)
import Eval (evalFile)
import System.Directory (doesFileExist)
import Data.Text.IO as TIO (readFile, putStrLn)
import Data.Text (pack)
import Options.Applicative 
    ( helper,
     execParser,
     strOption,
     short,
     progDesc,
     metavar,
     long,
     info,
     help,
     header,
     fullDesc,
     flag',
     Alternative ((<|>)),
     Parser)

-- run the given script
runScript :: FilePath -> IO ()
runScript filename = 
    do 
        ok <- doesFileExist filename
        if ok
        then TIO.readFile filename >>= evalFile filename
        else TIO.putStrLn $ "File '" <> pack filename <> "' does not exist"

data CrispOptions = REPLOptions | ScriptOptions String

parseOptions :: Parser CrispOptions
parseOptions = scriptOptions <|> replOptions
    where
        scriptOptions =
            ScriptOptions <$> strOption (long "script"
                                         <> short 's'
                                         <> metavar "FILENAME"
                                         <> help "file to execute")
        replOptions = 
            REPLOptions <$ flag' () (long "repl"
                                     <> short 'r'
                                     <> help "launch an interactive repl")

entryPoint :: CrispOptions -> IO ()
entryPoint REPLOptions = replLoop
entryPoint (ScriptOptions script) = runScript script

main :: IO ()
main = execParser options >>= entryPoint
    where 
        options = info (helper <*> parseOptions)
                ( fullDesc
                <> header "Crisp"
                <> progDesc "Executes script or launches an repl")