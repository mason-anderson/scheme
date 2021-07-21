{-# LANGUAGE OverloadedStrings #-}

module Prim where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception hiding (handle)
import Control.Monad
import Control.Monad.Trans
import System.Directory
import System.IO

import LispVal

type Prim   = [(T.Text, LispVal)]
type Unary  = LispVal -> Eval LispVal
type Binary = LispVal -> LispVal -> Eval LispVal

mkFun :: ([LispVal] -> Eval LispVal) -> LispVal
mkFun = Fun . IFunc

-- list of all primitive funcitons
primEnv :: Prim
primEnv =
    [ ("+"     , mkFun $ binOpFold (numOp (+)) (Number 0) )
    , ("-"     , mkFun $ binOp $    numOp    (-))
    , ("*"     , mkFun $ binOpFold (numOp    (*))  (Number 1) )
    , ("++"    , mkFun $ binOpFold (strOp    (<>)) (String ""))
    , ("-"     , mkFun $ binOp $    numOp    (-))
    , ("<"     , mkFun $ binOp $    numCmp   (<))
    , ("<="    , mkFun $ binOp $    numCmp   (<=))
    , (">"     , mkFun $ binOp $    numCmp   (>))
    , (">="    , mkFun $ binOp $    numCmp   (>=))
    , ("=="    , mkFun $ binOp $    numCmp   (==))
    , ("even?" , mkFun $ unOp  $    numBool   even)
    , ("odd?"  , mkFun $ unOp  $    numBool   odd)
    , ("pos?"  , mkFun $ unOp  $    numBool (< 0))
    , ("neg?"  , mkFun $ unOp  $    numBool (> 0))
    , ("null?" , mkFun $ unOp (eqCmd Nil) )
    , ("eq?"   , mkFun $ binOp   eqCmd )
    , ("nil?"  , mkFun   nil)
    , ("bl-eq?", mkFun $ binOp $ eqOp     (==))
    , ("and"   , mkFun $ binOpFold (eqOp     (&&)) (Bool True))
    , ("or"    , mkFun $ binOpFold (eqOp     (||)) (Bool False))
    , ("cons"  , mkFun   cons)
    , ("cdr"   , mkFun   cdr)
    , ("car"   , mkFun   car)
    , ("file?" , mkFun $ unOp  fileExists)
    , ("slurp" , mkFun $ unOp  slurp)
    , ("display", mkFun $ unOp display)
    , ("newline", mkFun $ newline)
    ]

unOp :: Unary -> [LispVal] -> Eval LispVal
unOp op [x] = op x
unOp _ args = throw $ NumArgs 1 args

binOp :: Binary -> [LispVal] -> Eval LispVal
binOp op [x,y]  = op x y
binOp _  args   = throw $ NumArgs 2 args

binOpFold :: Binary -> LispVal -> [LispVal] -> Eval LispVal
binOpFold op farg args = case args of
                           [a,b] -> op a b
                           (_:_) -> foldM op farg args
                           [] -> throw $ NumArgs 2 args


numBool :: (Integer -> Bool) -> LispVal -> Eval LispVal
numBool op (Number x) = return $ Bool $ op x
numBool _  x         = throw $ TypeMismatch "numeric op " x

numOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
numOp op (Number x) (Number y) = return $ Number $ op x  y
numOp _ x          (Number _) = throw $ TypeMismatch "numeric op " x
numOp _ (Number _)  y         = throw $ TypeMismatch "numeric op " y
numOp _ x           _         = throw $ TypeMismatch "numeric op " x

strOp :: (T.Text -> T.Text -> T.Text) -> LispVal -> LispVal -> Eval LispVal
strOp op (String x) (String y) = return $ String $ op x y
strOp _ x          (String _) = throw $ TypeMismatch "string op " x
strOp _ (String _)  y         = throw $ TypeMismatch "string op " y
strOp _ x           _         = throw $ TypeMismatch "string op " x

eqOp :: (Bool -> Bool -> Bool) -> LispVal -> LispVal -> Eval LispVal
eqOp op (Bool x) (Bool y) = return $ Bool $ op x y
eqOp _  x       (Bool _) = throw $ TypeMismatch "bool op " x
eqOp _ (Bool _)  y       = throw $ TypeMismatch "bool op " y
eqOp _ x         _       = throw $ TypeMismatch "bool op " x

numCmp :: (Integer -> Integer -> Bool) -> LispVal -> LispVal -> Eval LispVal
numCmp op (Number x) (Number y) = return . Bool $ op x  y
numCmp _ x          (Number _) = throw $ TypeMismatch "numeric op " x
numCmp _ (Number _)  y         = throw $ TypeMismatch "numeric op " y
numCmp _ x         _           = throw $ TypeMismatch "numeric op " x

eqCmd :: LispVal -> LispVal -> Eval LispVal
eqCmd (Atom   x) (Atom   y) = pure . Bool $ x == y
eqCmd (Number x) (Number y) = pure . Bool $ x == y
eqCmd (String x) (String y) = pure . Bool $ x == y
eqCmd (Bool   x) (Bool   y) = pure . Bool $ x == y
eqCmd  Nil        Nil       = pure $ Bool True
eqCmd  _          _         = pure $ Bool False

-- IO functions
fileExists :: LispVal -> Eval LispVal
fileExists (Atom a) = fileExists $ String a
fileExists (String str) = Bool <$> liftIO (doesFileExist $ T.unpack str)
fileExists val = throw $ TypeMismatch "expects str, got: " val

slurp :: LispVal -> Eval LispVal
slurp (String str) = liftIO $ wFileSlurp str
slurp val = throw $ TypeMismatch "expects str, got: " val

wFileSlurp :: T.Text -> IO LispVal
wFileSlurp fileName = withFile (T.unpack fileName) ReadMode go
    where go = readTextFile fileName

readTextFile :: T.Text -> Handle -> IO LispVal
readTextFile fileName handle = do
    exists <- doesFileExist $ T.unpack fileName
    if exists
    then TIO.hGetContents handle >>= (pure . String)
    else throw $ IOError $ T.concat [" file does not exits: ", fileName]

display :: LispVal -> Eval LispVal
display s@(String str) = liftIO ( TIO.putStr str) >> pure s
display x = liftIO ( TIO.putStr (showVal x)) >> pure x

newline :: [LispVal] -> Eval LispVal
newline [] = liftIO (TIO.putStrLn "") >> pure Nil
newline args = throw $ NumArgs 0 args

cons :: [LispVal] -> Eval LispVal
cons [x,List yList] = pure $ List $ x : yList
cons [c]              = pure $ List [c]
cons []               = pure $ List []
cons _  = throw $ ExpectedList "cons, in second argumnet"

car :: [LispVal] -> Eval LispVal
car [List []    ] = pure Nil
car [List (x:_)]  = pure x
car []            = pure Nil
car _             = throw $ ExpectedList "car"

cdr :: [LispVal] -> Eval LispVal
cdr [List (_:xs)] = pure $ List xs
cdr [List []]     = pure Nil
cdr []            = pure Nil
cdr _             = throw $ ExpectedList "cdr"

nil :: [LispVal] -> Eval LispVal
nil [List []] = pure $ Bool True
nil [List _] = pure $ Bool False
nil [Nil] = pure $ Bool True
nil _ = throw $ ExpectedList "nil? in first argument"
