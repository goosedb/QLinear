module Internal.Quasi.Parser (module Parsec, module Char, Parser, parse, var, expr, satisfyOneOf, char') where

import Control.Monad
import Data.Char as Char
import Language.Haskell.Exts.Parser (ParseResult (..), parseExp)
import Language.Haskell.Meta.Syntax.Translate (toExp)
import Language.Haskell.TH.Syntax
import Text.Parsec as Parsec hiding (parse)
import qualified Text.Parsec as P
import Text.Parsec.Char as Parsec
import Text.Parsec.Combinator as Parsec
import Text.Parsec.Error as Parsec

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

var :: Parser String
var = ((many1 $ satisfyOneOf outer) <> (many $ satisfyOneOf inner))
  where
    outer = [isAlpha, (== '_')]
    inner = isDigit : (== '\'') : outer
