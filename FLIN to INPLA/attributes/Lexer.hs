-- now with attributes

{-# LANGUAGE FlexibleContexts #-}

module Lexer
  -- ( Token(..)
  -- , lexer
  -- ) 
  where

import Control.Monad (guard)
import Data.Char (isAlphaNum, isLower, isUpper, isDigit)
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
    | TInteger Integer    -- integer literals
    | TPlus               -- '+'
    | TMinus              -- '-'
    | TStar               -- '*'
    | TSlash              -- '/'
    | TPercent            -- '%'
    | TOpenParen          -- '('
    | TCloseParen         -- ')'
    | TOpenSqParen        -- '['
    | TCloseSqParen       -- ']'
    | TComma              -- ','
    | TPar                -- '|'
    | TBlank              -- '_'
    | TDot                -- '.'
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
  , symbol '+'  >> pure TPlus
  , symbol '-'  >> pure TMinus
  , symbol '*'  >> pure TStar
  , symbol '/'  >> pure TSlash
  , symbol '%'  >> pure TPercent
  , symbol '('  >> pure TOpenParen
  , symbol ')'  >> pure TCloseParen
  , symbol '['  >> pure TOpenSqParen
  , symbol ']'  >> pure TCloseSqParen
  , symbol ','  >> pure TComma
  , symbol '|'  >> pure TPar
  , symbol '_'  >> pure TBlank
  , symbol '.'  >> pure TDot
  , integerLit
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

integerLit :: Parser Token
integerLit = do
  digits <- many1 (satisfy isDigit)
  pure (TInteger (read digits))

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
-- fixConstructors :: [Token] -> [Token]
-- fixConstructors [] = []
-- fixConstructors (TUpperID s : rest) =
--   case rest of
--     (TOpenParen : _) -> TUpperID s : fixConstructors rest
--     _                -> TUpperID s : TOpenParen : TCloseParen : fixConstructors rest
-- fixConstructors (t : ts) = t : fixConstructors ts
fixConstructors :: [Token] -> [Token]
fixConstructors = go
  where
    go [] = []

    -- Constructor with explicit arguments: leave unchanged
    go (TUpperID s : TOpenParen : ts) =
      TUpperID s : TOpenParen : go ts

    -- Constructor with attribute
    go (TUpperID s : TDot : ts) =
      let (attr, rest) = takeAttrSegment ts
      in case rest of
           -- Attribute followed by arguments ⇒ non-nullary constructor
           (TOpenParen : _) ->
             TUpperID s : TDot : attr ++ go rest

           -- Attribute not followed by arguments ⇒ nullary constructor
           _ ->
             TUpperID s : TDot : attr ++ [TOpenParen, TCloseParen] ++ go rest

    -- Plain nullary constructor
    go (TUpperID s : ts) =
      TUpperID s : TOpenParen : TCloseParen : go ts

    -- Other tokens
    go (t : ts) =
      t : go ts

    ----------------------------------------------------------------
    -- Attribute segment: tokens after '.' up to (but not including)
    -- '(' OR a top-level delimiter (e.g. ',', ')', ']', '=', 'in', '|')
    -- taking nesting into account.
    ----------------------------------------------------------------
    takeAttrSegment :: [Token] -> ([Token], [Token])
    takeAttrSegment = loop 0 0
      where
        loop _ _ [] = ([], [])
        loop p b all@(t:ts)
          | t == TOpenParen = ([], all)
          | p == 0 && b == 0 && isStop t = ([], all)
          | otherwise =
              let (p', b')      = step p b t
                  (xs, rest)    = loop p' b' ts
              in (t : xs, rest)

        step p b tok = case tok of
          TOpenParen    -> (p + 1, b)
          TCloseParen   -> (max 0 (p - 1), b)
          TOpenSqParen  -> (p, b + 1)
          TCloseSqParen -> (p, max 0 (b - 1))
          _             -> (p, b)

        isStop tok = case tok of
          TComma        -> True
          TCloseParen   -> True
          TCloseSqParen -> True
          TEquals       -> True
          TIn           -> True
          TPar          -> True
          _             -> False






-- ----------------------------------------------------------------
-- API
-- ----------------------------------------------------------------
lexer :: String -> Either ParseError [Token]
lexer input = case parse (ws *> many (lexToken <* ws) <* eof) "<lexer>" input of
    Left err   -> Left err
    Right toks -> Right (fixConstructors toks)