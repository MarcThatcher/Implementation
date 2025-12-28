{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Parser where

import           Text.Parsec            (Parsec, parse, eof, (<|>), (<?>), sepBy, try, lookAhead)
import           Text.Parsec.Combinator (chainl1)
import           Text.Parsec.Prim       (tokenPrim)
import           Text.Parsec.Error      (ParseError)
import           Text.Parsec.Pos        (SourcePos)
import           Lexer                  (Token(..), lexer)

----------------------------------------------------------
-- FLIN Term Parser
----------------------------------------------------------
----------------------------------------------------------
-- Types
----------------------------------------------------------
type VarName    = String
type ConstrName = String
type FuncName   = String


data Term
  = Empty
  | Var VarName
  | Constr ConstrName [Term]
  | Func FuncName [Term]
  | Let Term [VarName] Term
  | Par Term Term
  deriving (Eq,Show)

----------------------------------------------------------
-- Parser type and helpers
----------------------------------------------------------
type Parser a = Parsec [Token] () a

updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos pos _ _ = pos

tok :: (Token -> Maybe a) -> Parser a
tok test = tokenPrim show updatePos test

----------------------------------------------------------
-- Token-specific parsers
----------------------------------------------------------
lowerID :: Parser VarName
lowerID = tok (\case TLowerID s -> Just s; _ -> Nothing) <?> "lower identifier"

tOpenPar :: Parser ()
tOpenPar  = tok (\case TOpenParen  -> Just (); _ -> Nothing) <?> "("

tClosePar :: Parser ()
tClosePar = tok (\case TCloseParen -> Just (); _ -> Nothing) <?> ")"

tComma :: Parser ()
tComma    = tok (\case TComma      -> Just (); _ -> Nothing) <?> ","

tLet :: Parser ()
tLet = tok (\case TLet -> Just (); _ -> Nothing) <?> "let"

tIn :: Parser ()
tIn = tok (\case TIn -> Just (); _ -> Nothing) <?> "in"

tTilde :: Parser ()
tTilde = tok (\case TTilde -> Just (); _ -> Nothing) <?> "~"

tOpenSqParen :: Parser ()
tOpenSqParen = tok (\case TOpenSqParen -> Just (); _ -> Nothing) <?> "["

tCloseSqParen :: Parser ()
tCloseSqParen = tok (\case TCloseSqParen -> Just (); _ -> Nothing) <?> "]"

----------------------------------------------------------
-- Term parsers
----------------------------------------------------------
emptyTerm :: Parser Term
emptyTerm = tok (\case TBlank -> Just Empty; _ -> Nothing) <?> "empty"

varTerm :: Parser Term
varTerm = Var <$> lowerID

constrTerm :: Parser Term
constrTerm = do
  name <- tok (\case TUpperID s -> Just s; _ -> Nothing) <?> "constructor name"
  tOpenPar
  terms <- program `sepBy` tComma
  tClosePar
  pure (Constr name terms)

funcTerm :: Parser Term
funcTerm = try $ do
  name <- lowerID
  lookAhead tOpenPar   -- check that next token is '(' without consuming it
  tOpenPar
  terms <- program `sepBy` tComma
  tClosePar
  pure (Func name terms)

varList :: Parser [VarName]
varList = do
  tOpenSqParen
  names <- lowerID `sepBy` tComma
  tCloseSqParen
  pure names

letTerm :: Parser Term
-- e.g. let [x1,x2]~dup(x) in add(x1,x2)
letTerm = do
  tLet
  vars <- varList    -- moved before equals
  tTilde
  expr1 <- program   -- moved after equals  
  tIn
  expr2 <- program
  pure (Let expr1 vars expr2)

parOp :: Parser (Term -> Term -> Term)
parOp = do
  _ <- tok (\case TPar -> Just (); _ -> Nothing) <?> "'|'"
  pure Par

baseTerm :: Parser Term
baseTerm =
      letTerm
  <|> constrTerm
  <|> funcTerm
  <|> emptyTerm
  <|> varTerm
  <?> "term"

----------------------------------------------------------
-- parser with left-associative parallel terms
----------------------------------------------------------
program :: Parser Term
program = chainl1 baseTerm parOp <?> "term"

----------------------------------------------------------
-- FLIN Net Parser
----------------------------------------------------------
parseTerm :: String -> Either ParseError Term
parseTerm src = do
  tokens <- lexer src
  parse (program <* eof) "<tokens>" tokens


----------------------------------------------------------
-- FLIN Rule Parser
----------------------------------------------------------
data Rule = Rule Term Term deriving (Eq,Show)

tEquals :: Parser ()
tEquals = tok (\case TEquals -> Just (); _ -> Nothing) <?> "="

ruleParser :: Parser Rule
ruleParser = do
  lhs <- program
  tEquals
  rhs <- program
  pure (Rule lhs rhs)

----------------------------------------------------------
-- API
----------------------------------------------------------
parseRule :: String -> Either ParseError Rule
parseRule src = do
  tokens <- lexer src
  parse (ruleParser <* eof) "<tokens>" tokens

