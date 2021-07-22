{-# LANGUAGE OverloadedStrings #-}

module Repl where

import Data.Text as T
import Control.Monad.Trans
import System.Console.Haskeline

import Eval
import LispVal

type Repl a = InputT IO a

mainLoop :: IO ()
mainLoop = do
    env <- getStdEnv
    _ <- runInputT defaultSettings $ repl env
    pure ()

repl :: EnvCtx -> Repl EnvCtx
repl env = do
    minput <- getInputLine "λ> "
    case minput of
      Nothing -> outputStrLn "Goodbye." >> pure env
      Just input -> liftIO (process env input) >>= repl

process :: EnvCtx -> String -> IO EnvCtx
process env str = do
    res <- safeExec $ evalText env $ T.pack str
    case res of
        Left err -> putStrLn err >> pure env
        Right env' -> pure env'


processToAST :: String -> IO ()
processToAST str = print $ runParseTest $ T.pack str
