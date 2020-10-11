{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Internal.Quasi.Matrix.Expression.Quote where

import Internal.Matrix
import qualified Internal.Quasi.Matrix.Expression.Parser as Parser
import qualified Internal.Quasi.Parser as Parser
import Internal.Quasi.Quasi
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

expr :: Parser.Parser [[Exp]] -> String -> Q Exp
expr parser source = do
  let (matrix, (m, n)) = unwrap $ parse source parser
  let sizeType = LitT . NumTyLit
  let constructor = foldl AppTypeE (ConE 'Matrix) [sizeType m, sizeType n, WildCardT]
  let size = TupE $ map (LitE . IntegerL) [m, n]
  let value = ListE $ map ListE $ matrix
  pure $ foldl AppE constructor [size, value]

parse :: String -> Parser.Parser [[a]] -> Either [String] ([[a]], (Integer, Integer))
parse source parser = do
  matrix <- Parser.parse parser "QLinear" source
  size <- checkSize matrix
  pure (matrix, size)

checkSize :: [[a]] -> Either [String] (Integer, Integer)
checkSize [] = Left ["Matrix cannot be empty"]
checkSize matrix =
  let lines@(l : ls) = map length matrix
   in if all (== l) ls
        then Right (fromIntegral $ length matrix, fromIntegral l)
        else Left ["All lines must be the same length"]
