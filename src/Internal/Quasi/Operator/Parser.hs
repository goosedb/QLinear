module Internal.Quasi.Operator.Parser where

import Internal.Quasi.Parser
import Language.Haskell.TH.Syntax

definition :: Parser ([Pat], [Exp])
definition = do
  params <- spaces *> parameters <* spaces
  _ <- string "=>"
  lams <- spaces *> lambdas <* spaces
  eof
  pure (params, lams)

lambdas :: Parser [Exp]
lambdas = do
  result <- many1 anyChar >>= expr
  case result of
    TupE elems -> pure elems
    err -> parserFail $ show err

parameters :: Parser [Pat]
parameters = char '(' *> inner <* char ')'
  where
    inner = map (VarP . mkName) <$> var `sepBy` (spaces >> char ',' >> spaces)
