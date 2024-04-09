{-# LANGUAGE OverloadedStrings #-}

module REPL (
    replLoop,
) where

import Eval (safeExec, evalText)
import Data.Text as T (pack)

import Control.Monad.Trans (MonadIO (liftIO))
import System.Console.Haskeline
    (defaultSettings, getInputLine, outputStrLn, runInputT, InputT)

type REPL a = InputT IO a

process :: String -> IO ()
process s = do
    res <- safeExec . evalText . T.pack $ s
    either putStrLn return res

repl :: REPL ()
repl = do
    input <- getInputLine "crisp>"
    case input of 
        Nothing -> outputStrLn "Sayonara"
        Just x -> liftIO (process x) >> repl

replLoop :: IO ()
replLoop = runInputT defaultSettings repl

