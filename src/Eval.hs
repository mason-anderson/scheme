{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import Control.Monad.State
import Control.Exception
import System.Directory

import LispVal
import Prim
import Parser

local :: (EnvCtx -> EnvCtx) -> Eval a -> Eval a
-- local update x = Eval $ withStateT update $ unEval x
local update x = do
    env <- get
    modify update
    a <- x
    put env
    pure a

basicEnv :: EnvCtx
basicEnv =  M.fromList $ primEnv 
         <> [("read", Fun $ IFunc $ unOp readFn)
            , ("parse", Fun $ IFunc $ unOp parseFn)
            , ("eval", Fun $ IFunc $ unOp eval)
            , ("show", Fun $ IFunc $ unOp (pure . String . showVal))
            ]

-- evaluate a program file
evalFile :: EnvCtx -> FilePath -> T.Text -> IO EnvCtx
evalFile env filePath fileExpr = do
    (_, env') <- runASTinEnv env (fileToEvalForm filePath fileExpr)
    pure env'

-- evaluate a line from the REPL
evalText :: EnvCtx -> T.Text -> IO EnvCtx
evalText env textExpr = do
    (res, env') <- runASTinEnv env $ textToEvalForm textExpr
    print res
    pure env'

textToEvalForm :: T.Text -> Eval LispVal
textToEvalForm input = either (throw . PError . show ) eval $ readExpr input

fileToEvalForm :: FilePath -> T.Text -> Eval LispVal
fileToEvalForm filePath input = either (throw . PError . show )
                                       evalBody
                                       $ readExprFile filePath input
stdLib :: FilePath
stdLib = "lib/stdlib.scm"

getStdEnv :: IO EnvCtx
getStdEnv = do
    file <- getFileContents stdLib
    evalFile basicEnv stdLib file

-- catch any exceptions and keep executing
safeExec :: IO a -> IO (Either String a)
safeExec m = do
  result <- Control.Exception.try m
  case result of
    Left (eTop :: SomeException) ->
      case fromException eTop of
        Just (enclosed :: LispException) -> return $ Left (show enclosed)
        Nothing                -> return $ Left (show eTop)
    Right val -> return $ Right val


getFileContents :: FilePath -> IO T.Text
getFileContents fname = do
  exists <- doesFileExist fname
  if exists then TIO.readFile  fname else return "File does not exist."

runParseTest :: T.Text -> T.Text -- for view AST
runParseTest input = either (T.pack . show)
                            (T.pack . show)
                            $ readExpr input

runASTinEnv :: EnvCtx -> Eval a -> IO (a, EnvCtx)
runASTinEnv env action = runStateT (unEval action) env

readFn :: LispVal -> Eval LispVal
readFn (String txt) = lineToEvalForm txt
readFn  val         = throw $ TypeMismatch "read expects string, instead got: " val

parseFn :: LispVal -> Eval LispVal
parseFn (String txt) = either (throw . PError . show) return $ readExpr txt
parseFn val = throw $ TypeMismatch "parse expects string, instead got: " val

lineToEvalForm :: T.Text -> Eval LispVal
lineToEvalForm input = either (throw . PError . show  )  eval $ readExpr input

eval :: LispVal -> Eval LispVal
eval a@(Atom _)   = getVar a
eval n@(Number _) = pure n
eval s@(String _) = pure s
eval b@(Bool _)   = pure b
eval Nil          = pure Nil
eval (List [])    = throw $ Default (List [])
eval l@(Lambda _ _) = pure l

eval (List [Atom "quote", val]) = pure val

eval (List [Atom "write", rest]) = pure . String . T.pack $ show rest
eval (List ((:) (Atom "write") rest)) = pure . String . T.pack . show $ List rest

eval (List [Atom "if", cond, truExpr, flsExpr]) = do
                     ifRes <- eval cond
                     case ifRes of
                       (Bool True)  -> eval truExpr
                       (Bool False) -> eval flsExpr
                       _            -> throw $ BadSpecialForm "if"
eval (List [Atom "let", List pairs, expr]) = do
    env <- get
    atoms <- mapM (\(List [a, _]) -> ensureAtom a) pairs
    vals <- mapM (\(List [_, v]) -> eval v) pairs
    let env' = M.fromList (zipWith (\a b -> (extractVar a, b)) atoms vals) <> env
    local (const env') $ eval expr

eval (List [Atom "begin", rest]) = evalBody rest
eval (List ((:) (Atom "begin") rest )) = evalBody $ List rest

eval (List [Atom "define", varExpr, expr]) = do
    varAtom <- ensureAtom varExpr
    evalVal <- eval expr
    modify $ M.insert (extractVar varAtom) evalVal
    pure varExpr

eval (List [Atom "lambda", List params, expr]) = do
    envLocal <- get
    pure $ Lambda (IFunc $ applyLambda expr params) envLocal
eval (List (Atom "lambda":_) ) = throw $ BadSpecialForm "lambda"

eval (List ((:) x xs)) = do
    env <- get
    funVar <- eval x
    xVal   <- mapM eval xs
    case funVar of
        (Fun (IFunc internalFn)) -> internalFn xVal
        (Lambda (IFunc internalfn) boundenv) -> local (const (boundenv <> env)) $ internalfn xVal
        _ -> throw $ NotFunction funVar
eval x = throw $ Default x

evalBody :: LispVal -> Eval LispVal
evalBody (List [List ((:) (Atom "define") [Atom var, defExpr]), rest]) = do
    evalVal <- eval defExpr
    modify $ M.insert var evalVal
    eval rest
evalBody (List ((:) (List ((:) (Atom "define") [Atom var, defExpr])) rest)) = do
    evalVal <- eval defExpr
    modify $ M.insert var evalVal
    evalBody $ List rest

evalBody (List [x]) = eval x
evalBody (List (x:xs)) = do
    _ <- eval x
    evalBody $ List xs
evalBody (List []) = pure Nil
evalBody x = eval x

applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
applyLambda expr params args = do
    env <- get
    let env' = M.fromList (Prelude.zipWith (\a b -> (extractVar a,b)) params args) <> env
    local (const env' ) $ eval expr

getVar :: LispVal -> Eval LispVal
getVar (Atom a) = do
    env <- get
    case M.lookup a env of
      Just x -> pure x
      Nothing -> throw $ UnboundVar a
getVar _ = undefined

ensureAtom :: LispVal -> Eval LispVal
ensureAtom a@(Atom _) = pure a
ensureAtom a = throw $ TypeMismatch "atom" a

extractVar :: LispVal -> T.Text
extractVar (Atom a) = a
extractVar _ = undefined

