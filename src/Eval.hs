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
local update x = Eval $ withStateT update $ unEval x

basicEnv :: EnvCtx
-- basicEnv = M.fromList $ primEnv <> [("read" , Fun $ IFunc $ unop $ readFn)]
basicEnv = M.fromList $ primEnv <> []

evalFile :: EnvCtx -> FilePath -> T.Text -> IO EnvCtx --program file
evalFile env filePath fileExpr = do 
    (_, env') <- runASTinEnv env (fileToEvalForm filePath fileExpr)
    pure env'

evalText :: EnvCtx -> T.Text -> IO EnvCtx
evalText env textExpr = do
    (res, env') <- runASTinEnv env $ textToEvalForm textExpr
    print res
    pure env'

textToEvalForm :: T.Text -> Eval LispVal
textToEvalForm input = either (throw . PError . show )  evalBody $ readExpr input

fileToEvalForm :: FilePath -> T.Text -> Eval LispVal
fileToEvalForm filePath input = either (throw . PError . show )
                              evalBody
                              $ readExprFile filePath input
stdLib :: FilePath
stdLib = "lib/stdlib.scm"

stdEnv :: IO EnvCtx
stdEnv = do
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
runASTinEnv code action = runStateT (unEval action) code


eval :: LispVal -> Eval LispVal
eval a@(Atom _)   = getVar a
eval n@(Number _) = pure n
eval s@(String _) = pure s
eval b@(Bool _)   = pure b
eval Nil          = pure Nil
eval (List [])    = throw $ Default (List [])

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
    local (const env') $ evalBody expr

eval (List [Atom "begin", rest]) = evalBody rest
eval (List ((:) (Atom "begin") rest )) = evalBody $ List rest

eval (List [Atom "define", varExpr, expr]) = do
    varAtom <- ensureAtom varExpr
    evalVal <- eval expr
    env <- get
    let envFn = const $ M.insert (extractVar varAtom) evalVal env
    local envFn $ pure varExpr

eval (List [Atom "lambda", List params, expr]) = do
    envLocal <- get
    pure $ Lambda (IFunc $ applyLambda expr params) envLocal
eval (List (Atom "lambda":_) ) = throw $ BadSpecialForm "lambda"

eval (List ((:) x xs)) = do
    funVar <- eval x
    xVal   <- mapM eval  xs
    case funVar of
        (Fun (IFunc internalFn)) -> internalFn xVal
        (Lambda (IFunc internalfn) boundenv) -> local (const boundenv) $ internalfn xVal
        _ -> throw $ NotFunction funVar
eval x = throw $ Default x

evalBody :: LispVal -> Eval LispVal
evalBody (List [List ((:) (Atom "define") [Atom var, defExpr]), rest]) = do
    evalVal <- eval defExpr
    env     <- get
    local (const $ M.insert var evalVal env) $ eval rest
evalBody (List ((:) (List ((:) (Atom "define") [Atom var, defExpr])) rest)) = do
    evalVal <- eval defExpr
    env     <- get
    let envFn = const $ M.insert var evalVal env
    local envFn $ evalBody $ List rest
evalBody x = eval x

applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
applyLambda expr params args = do
    env <- get
    argEval <- mapM eval args
    let env' = M.fromList (Prelude.zipWith (\a b -> (extractVar a,b)) params argEval) <> env
    local (const env' ) $ eval expr

getVar :: LispVal -> Eval LispVal
getVar (Atom a) = do
    env <- get
    case M.lookup a env of
      Just x -> return x
      Nothing -> throw $ UnboundVar a
getVar _ = undefined

ensureAtom :: LispVal -> Eval LispVal
ensureAtom a@(Atom _) = pure a
ensureAtom a = throw $ TypeMismatch "atom" a

extractVar :: LispVal -> T.Text
extractVar (Atom a) = a
extractVar _ = undefined

