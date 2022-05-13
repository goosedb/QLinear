module Internal.Quasi.Operator.Parser where

import Internal.Quasi.Parser
import Language.Haskell.TH.Syntax
import Data.Maybe

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
    TupE elems | all isJust elems -> pure (map fromJust elems)
    err -> parserFail $ show err

parameters :: Parser [Pat]
parameters = char '(' *> inner <* char ')'
  where
    inner = map (VarP . mkName) <$> var `sepBy` (spaces >> char ',' >> spaces)
