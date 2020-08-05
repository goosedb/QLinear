module Internal.Quasi.Operator.Parser where

import Language.Haskell.TH.Syntax

import Internal.Quasi.Parser

definition :: Parser ([Pat], [Exp])
definition = do
  params <- spaces *> parameters <* spaces
  string "=>"
  lams <- spaces *> lambdas <* spaces
  pure (params, lams)

lambdas :: Parser [Exp]
lambdas = do
  result <- many1 anyChar >>= expr
  case result of
    TupE elems -> pure elems
    otherwise -> parserFail $ show otherwise
    
parameters :: Parser [Pat]
parameters = map (VarP . mkName) <$> var `endBy1` spaces