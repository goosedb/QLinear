{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE KindSignatures #-}

module Internal.Quasi.Matrix.Pattern.Quote (pat) where

import Data.Proxy (Proxy (..))
import GHC.TypeNats (Nat)
import Language.Haskell.TH.Lib (listP, litT, numTyLit, varP)
import Language.Haskell.TH.Syntax (Pat, Q, mkName)
import Internal.Matrix (Matrix (..))

data MatrixPat (m :: Nat) (n :: Nat) a = MatrixPat (Proxy m) (Proxy n) [[a]]

matrixPatHelper :: Matrix m n a -> MatrixPat m n a
matrixPatHelper (Matrix _ elems) = MatrixPat Proxy Proxy elems

-- DRAFT. VERY DIRTY AND VERY RAW DRAFT

pat :: String -> Q Pat
pat raw =
  [p|
    (matrixPatHelper ->
      MatrixPat (Proxy :: Proxy $height) (Proxy :: Proxy $width) $ps)
  |]
  where
    ls = map words $ split ';' raw
    height = litT $ numTyLit $ fromIntegral $ length ls
    width = litT $ numTyLit $ fromIntegral $ length $ head ls
    ps = listP $ (listP . map (varP . mkName)) <$> ls

split :: Char -> String -> [String]
split sep = reverse . go [] [] where
  go res [] [] = res
  go res str [] = reverse str : res
  go res str (c:cs)
    | c == sep = go (reverse str: res) [] cs
    | otherwise = go res (c:str) cs
