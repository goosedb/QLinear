{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Internal.Quasi.Matrix.Quasi (matrix, vector) where

import Internal.Matrix
import qualified Internal.Quasi.Matrix.Parser as Parser
import qualified Internal.Quasi.Parser as Parser
import Internal.Quasi.Quasi
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

-- | Macro constructor for 'QLinear.Matrix.Matrix'
--
-- >>> [matrix| 1 2; 3 4 |]
-- [1,2]
-- [3,4]
-- >>> :t [matrix| 1 2; 3 4|]
-- [matrix| 1 2; 3 4|] :: Num a => Matrix 2 2 a
matrix :: QuasiQuoter
matrix =
  QuasiQuoter
    { quoteExp = expr Parser.matrix,
      quotePat = notDefined "Pattern",
      quoteType = notDefined "Type",
      quoteDec = notDefined "Declaration"
    }
  where
    notDefined = isNotDefinedAs "matrix"

-- | Macro constructor for 'QLinear.Matrix.Vector'.
--
-- >>> [vector| 1 2 3 4 |]
-- [1]
-- [2]
-- [3]
-- [4]
-- >>> :t [vector| 1 2 3 4 |]
-- [vector| 1 2 3 4 |] :: Num a => Vector 4 a
vector :: QuasiQuoter
vector =
  QuasiQuoter
    { quoteExp = vectorExpr,
      quotePat = notDefined "Pattern",
      quoteType = notDefined "Type",
      quoteDec = notDefined "Declaration"
    }
  where
    notDefined = isNotDefinedAs "vector"

vectorExpr :: String -> Q Exp
vectorExpr source = f <$> expr Parser.vector source
  where
    f = AppE (VarE 'toVector)

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

toVector :: Matrix n 1 a -> Vector n a
toVector = id
