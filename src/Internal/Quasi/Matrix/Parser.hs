module Internal.Quasi.Matrix.Parser where

import Language.Haskell.TH.Syntax

import Internal.Quasi.Parser

matrix :: Parser [[Exp]]
matrix = line `sepBy` char ';'

vector :: Parser [[Exp]]
vector = map pure <$> (line <* eof)

line :: Parser [Exp]
line = spaces >> unit `endBy1` spaces

unit :: Parser Exp
unit = (var <|> num <|> inBrackets) >>= expr
  
var :: Parser String
var = ((many1 $ satisfyOneOf outer) <> (many $ satisfyOneOf inner)) where
  outer = [isAlpha, (== '_')]
  inner = isDigit : (=='\'') :outer

num :: Parser String
num = do 
  neg <- (char' '-') <|> pure []
  beforeDot <- ((many1 outer <> many inner) <|> char' '0') 
  afterDot <- char' '.' <> many1 inner <|> mempty 
  pure $ neg <> beforeDot <> afterDot
    where
      outer = oneOf ['1'..'9']
      inner = char '0' <|> outer

inBrackets :: Parser String
inBrackets = nested '(' ')'

nested :: Char -> Char -> Parser String
nested open close = char' open <> scan 1 where
  scan 0 = pure mempty
  scan n = many (noneOf [open, close]) <> (char' open <> inc n <|> char' close <> dec n)
  inc = scan . (+1)
  dec = scan . (\n -> n - 1)
