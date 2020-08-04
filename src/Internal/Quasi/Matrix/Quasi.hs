{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Internal.Quasi.Matrix.Quasi (matrix) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import qualified Internal.Quasi.Matrix.Parser as Parser
import qualified Internal.Quasi.Parser as Parser

import qualified Data.List as List

import Internal.Matrix

matrix :: QuasiQuoter 
matrix = QuasiQuoter 
  { quoteExp = expr
  , quotePat = isNotDefinedAs "Pattern"
  , quoteType = isNotDefinedAs "Type"
  , quoteDec = isNotDefinedAs "Declaration"
  }

isNotDefinedAs :: String -> a
isNotDefinedAs as = error $ "You cannot use matrix quasi as " <> as

expr :: String -> Q Exp
expr source = do
  let (matrix, (m, n)) = unwrap do 
      matrix <- Parser.parse Parser.matrix "QMatrix" source
      size <- checkSize matrix
      pure (matrix, size)
  let sizeType = LitT . NumTyLit
  let constructor = AppTypeE (AppTypeE (AppTypeE (ConE 'Matrix) (sizeType m)) (sizeType n)) WildCardT
  let size = TupE $ map (LitE . IntegerL) [m, n]
  let value = ListE $ map ListE $ matrix
  pure $ AppE (AppE constructor size) value

checkSize :: [[a]] -> Either [String] (Integer, Integer)
checkSize [] = Left ["Matrix cannot be empty"]
checkSize matrix = 
  let lines@(l:ls) = map length matrix 
  in if all (== l) ls then Right (fromIntegral $ length matrix, fromIntegral l) 
                      else Left ["All lines must be the same length"]

unwrap :: Either [String] a -> a
unwrap (Left a) = error $ List.intercalate ", " a
unwrap (Right a ) = a
