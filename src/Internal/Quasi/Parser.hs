module Internal.Quasi.Parser (module Parsec, module Char, Parser, parse, expr, satisfyOneOf, char') where

import Text.Parsec as Parsec hiding (parse)
import Text.Parsec.Char as Parsec
import Text.Parsec.Combinator as Parsec
import Text.Parsec.Error as Parsec
import Data.Char as Char

import qualified Text.Parsec as P

import Language.Haskell.Exts.Parser (parseExp, ParseResult(..))
import Language.Haskell.Meta.Syntax.Translate (toExp)

import Language.Haskell.TH.Syntax

import Control.Monad

type Parser a = Parsec String () a

parse :: Parser a -> SourceName -> String -> Either [String] a
parse parser name source = case P.parse parser name source of 
  Right a -> Right a
  Left err -> Left $ map messageString $ errorMessages $ err

satisfyOneOf :: [Char -> Bool] -> Parser Char
satisfyOneOf ps = satisfy (or . ap ps . pure)

expr :: String -> Parser Exp
expr source = 
  case parseExp source of 
    ParseOk exp -> pure $ toExp exp
    ParseFailed _ e -> parserFail e 

char' :: Char -> Parser String
char' = fmap pure . char 

anyChar' :: Parser String 
anyChar' = fmap pure $ anyChar 

