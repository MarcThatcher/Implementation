{-# LANGUAGE FlexibleContexts #-}

module Lexer
  -- ( Token(..)
  -- , lexer
  -- ) 
  where

import Control.Monad (guard)
import Data.Char (isAlphaNum, isLower, isUpper)
import Text.Parsec
import Text.Parsec.String (Parser)

-- ----------------------------------------------------------------
-- Tokens
-- ----------------------------------------------------------------

data Token
    = TEquals             -- '='
    | TTilde              -- '~'
    | TLet                -- 'let'
    | TIn                 -- 'in'
    | TLowerID String
    | TUpperID String
    | TOpenParen          -- '('
    | TCloseParen         -- ')'
    | TOpenSqParen        -- '['
    | TCloseSqParen       -- ']'
    | TComma              -- ','
    | TPar                -- '|'
    | TBlank              -- '_'
    deriving (Eq, Show)

-- ----------------------------------------------------------------
-- Lexer
-- ----------------------------------------------------------------
lexToken :: Parser Token
lexToken = choice
  [ reservedLet
  , reservedIn
  , symbol '='  >> pure TEquals
  , symbol '~'  >> pure TTilde
  , symbol '('  >> pure TOpenParen
  , symbol ')'  >> pure TCloseParen
  , symbol '['  >> pure TOpenSqParen
  , symbol ']'  >> pure TCloseSqParen
  , symbol ','  >> pure TComma
  , symbol '|'  >> pure TPar
  , symbol '_'  >> pure TBlank
  , lowerIdent
  , upperIdent
  ] <?> "token"

-- ----------------------------------------------------------------
-- Pieces
-- ----------------------------------------------------------------
reservedIn :: Parser Token
reservedIn = try $ do
  _ <- string "in"
  notFollowedBy (satisfy isIdentChar)
  pure TIn

reservedLet :: Parser Token
reservedLet = try $ do
  _ <- string "let"
  notFollowedBy (satisfy isIdentChar)
  pure TLet

lowerIdent :: Parser Token
lowerIdent = try $ do
  x  <- satisfy isLower
  xs <- many (satisfy isIdentChar)
  let s = x:xs
  guard (s /= "let" && s /= "in")  -- exclude both reserved words
  pure (TLowerID s)

upperIdent :: Parser Token
upperIdent = do
  x  <- satisfy isUpper
  xs <- many (satisfy isIdentChar)
  pure (TUpperID (x:xs))

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

symbol :: Char -> Parser Char
symbol = char

ws :: Parser ()
ws = skipMany (oneOf " \t\r\n")

-- ----------------------------------------------------------------
-- add () to nullary constructors 
-- ----------------------------------------------------------------
fixConstructors :: [Token] -> [Token]
fixConstructors [] = []
fixConstructors (TUpperID s : rest) =
  case rest of
    (TOpenParen : _) -> TUpperID s : fixConstructors rest
    _                -> TUpperID s : TOpenParen : TCloseParen : fixConstructors rest
fixConstructors (t : ts) = t : fixConstructors ts


-- ----------------------------------------------------------------
-- API
-- ----------------------------------------------------------------
lexer :: String -> Either ParseError [Token]
lexer input = case parse (ws *> many (lexToken <* ws) <* eof) "<lexer>" input of
    Left err   -> Left err
    Right toks -> Right (fixConstructors toks)