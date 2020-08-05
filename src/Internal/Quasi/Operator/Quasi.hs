{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Internal.Quasi.Operator.Quasi where

import Data.List.Split (chunksOf)
import Data.Proxy

import GHC.TypeNats
import qualified GHC.Natural as Natural

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import qualified Internal.Quasi.Operator.Parser as Parser
import qualified Internal.Quasi.Parser as Parser
import Internal.Quasi.Quasi

import Internal.Identity
import Internal.Matrix

operator :: QuasiQuoter 
operator = QuasiQuoter 
  { quoteExp = expr
  , quotePat = notDefined "Pattern"
  , quoteType = notDefined "Type"
  , quoteDec = notDefined "Declaration"
  } where notDefined = isNotDefinedAs "operator" 


expr :: String -> Q Exp
expr source = do
  let (params, lams, n) = unwrap do 
      (params, lams) <- Parser.parse Parser.definition "QLinear" source
      size <- checkSize (params, lams)
      pure (params, lams, size)
  let sizeType = LitT . NumTyLit
  let size = TupE $ map (LitE . IntegerL) [n, 1]
  let func = VarE $ mkName "matrixOfOperator"
  let constructor = foldl AppTypeE (ConE 'Matrix) [sizeType n, sizeType 1, WildCardT]
  let value = ListE $ map (ListE. pure . LamE [ListP params]) lams
  pure $ AppE func $ foldl AppE constructor [size, value]

checkSize :: ([Pat], [Exp]) -> Either [String] Integer
checkSize ([], _) = Left ["Parameters of operator cannot be empty"]
checkSize (_, []) = Left ["Body of operator cannot be empty"]
checkSize (names, exprs) = 
  let namesLength = length names
      exprsLength = length exprs
  in if namesLength == exprsLength then Right $ fromIntegral namesLength 
     else Left ["Number of arguments and number of lambdas must be equals"]

matrixOfOperator :: forall n a b. (KnownNat n, HasIdentity a) => Matrix n 1 ([a] -> b) -> Matrix n n b
matrixOfOperator (Matrix _ fs) = Matrix (n, n) $ chunksOf n [ f line | f <- concat fs, line <- identity ] where
  (Matrix _ identity) = e :: Matrix n n a
  n = Natural.naturalToInt $ natVal (Proxy @n)