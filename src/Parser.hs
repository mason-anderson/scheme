{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Text.Parsec
import Text.Parsec.Text
-- import Text.Parsec.Expr
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import qualified Data.Text as T
-- import Control.Applicative hiding ((<|>))
import Data.Functor.Identity (Identity)

import LispVal

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef
    { Tok.commentStart = "{-"
    , Tok.commentEnd = "-}"
    , Tok.commentLine = ";"
    , Tok.opStart = Tok.opLetter style
    , Tok.opLetter = oneOf ":!#$%%&*+./<=>?@\\^|-~"
    , Tok.identStart = letter <|>  oneOf "-+/*=|&><"
    , Tok.identLetter = digit <|> letter <|> oneOf "?+=|&-/"
    , Tok.reservedOpNames = [ "'", "\""]
    }

Tok.TokenParser { Tok.parens = m_parens
                , Tok.identifier = m_identifier
                } = Tok.makeTokenParser style

reservedOp :: T.Text -> Parser ()
reservedOp op = Tok.reservedOp lexer $ T.unpack op

parseAtom :: Parser LispVal
parseAtom = do
    p <- m_identifier
    pure $ Atom $ T.pack p

parseText :: Parser LispVal
parseText = do
    reservedOp "\""
    p <- many1 $ noneOf "\""
    reservedOp "\""
    pure $ String . T.pack $ p

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseNegNum :: Parser LispVal
parseNegNum = do
    _ <- char '-'
    d <- many1 digit
    pure $ Number . negate . read $ d

parseList :: Parser LispVal
parseList = List . concat <$> m_parens (Text.Parsec.many parseExpr `sepBy` (char ' ' <|> char '\n'))

parseSExp :: Parser LispVal
parseSExp = List . concat <$> m_parens (Text.Parsec.many parseExpr `sepBy` (char ' ' <|> char '\n'))

parseQuote :: Parser LispVal
parseQuote = do
    reservedOp "\'"
    x <- parseExpr
    pure $ List [Atom "quote", x]

parseReserved :: Parser LispVal
parseReserved = do
    reservedOp "Nil" >> pure Nil
    <|> (reservedOp "#t" >> pure (Bool True))
    <|> (reservedOp "#f" >> pure (Bool False))

parseExpr :: Parser LispVal
parseExpr =  parseReserved
         <|> parseNumber
         <|> try parseNegNum
         <|> parseAtom
         <|> parseText
         <|> parseQuote
         <|> parseSExp

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    pure r

readExpr :: T.Text -> Either ParseError LispVal
readExpr = parse (contents parseExpr) "<stdin>"

readExprFile :: SourceName -> T.Text -> Either ParseError LispVal
readExprFile = parse (contents parseList)
