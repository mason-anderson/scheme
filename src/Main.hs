{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Repl
import Eval

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> mainLoop
      [fileName] -> do
          file <- TIO.readFile fileName
          env <- getStdEnv
          _ <- evalFile env fileName file
          pure ()
      _ -> putStrLn "too many arguments"
