{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Internal.Quasi.Matrix.Expression.Quote where

import Data.List.NonEmpty (NonEmpty((:|)))
import Internal.Matrix
import qualified Internal.Quasi.Parser as Parser
import Internal.Quasi.Quasi
import Language.Haskell.TH.Syntax

expr :: Parser.Parser [[Exp]] -> String -> Q Exp
expr parser source = do
  let (matrix, (m, n)) = unwrap $ parse source parser
  let sizeType = LitT . NumTyLit
  let constructor = foldl AppTypeE (ConE 'Matrix) [sizeType m, sizeType n, WildCardT]
  let msize = TupE $ map (Just . LitE . IntegerL) [m, n]
  let mvalue = ListE $ map ListE matrix
  pure $ foldl AppE constructor [msize, mvalue]

parse :: String -> Parser.Parser [[a]] -> Either [String] ([[a]], (Integer, Integer))
parse source parser = do
  matrix <- Parser.parse parser "QLinear" source
  msize <- checkSize matrix
  pure (matrix, msize)

checkSize :: [[a]] -> Either [String] (Integer, Integer)
checkSize [] = Left ["Matrix cannot be empty"]
checkSize matrix@(m : ms) =
  let l :| ls = length <$> m :| ms
   in if all (== l) ls
        then Right (fromIntegral $ length matrix, fromIntegral l)
        else Left ["All lines must be the same length"]
