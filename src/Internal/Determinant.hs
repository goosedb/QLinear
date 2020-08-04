{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Internal.Determinant where

import Data.Proxy
import qualified Data.List as List

import GHC.TypeNats
import qualified GHC.Natural as Natural

import Internal.Matrix
import Internal.Index

det :: Num a => Matrix n n a -> a
det (Matrix (1, _) [[a]]) = a
det (Matrix (2, _) [[a, b], [c, d]]) = a * d - b * c
det (Matrix (3, _) [[a, b, c], [d, e, f], [g, h, k]]) =
    a * e * k + b * f * g + c * d * h - c * e * g - b * d * k - a * f * h
det (Matrix (n, _) matrix) = sum $ map (unsafeAlgebraicComplement matrix n) $ zip [1, 1..] indices where
  indices = [1..n]
      
algebraicComplement :: forall n a i j.(KnownNat i, KnownNat j, KnownNat n, Num a, i <= n, j <= n) => Matrix n n a -> Index i j -> a
algebraicComplement (Matrix (n, _) matrix) _ = unsafeAlgebraicComplement matrix n (i, j) where
  i = (Natural.naturalToInt $ natVal $ Proxy @i)
  j = (Natural.naturalToInt $ natVal $ Proxy @j)

algebraicComplement' :: Num a => Matrix n n a -> (Int, Int) -> Maybe a
algebraicComplement' (Matrix (n, _) matrix) ij@(i, j) 
  | i <= n && j <= n = Just $ unsafeAlgebraicComplement matrix n ij
  | otherwise = Nothing

unsafeAlgebraicComplement :: forall a. Num a => [[a]] -> Int -> (Int, Int) -> a
unsafeAlgebraicComplement matrix n (i, j) = aij * k * det (Matrix (n - 1, n - 1) minor) where
  (aij, minor) = cross matrix (i - 1, j - 1) 
  k = fromIntegral $ (-1) ^ (j + i)

cross :: [[a]] -> (Int, Int) -> (a, [[a]])
cross matrix (i, j) = (line !! j, withoutColumn) where
  (line, withoutLine) = cut' i matrix 
  withoutColumn = map (cut j) withoutLine

cut' :: Int -> [a] -> (a, [a])
cut' 0 (x:xs) = (x, xs)
cut' n (x:xs) = (x :) <$> cut' (n - 1) xs 

cut :: Int -> [a] -> [a]
cut 0 (x:xs) = xs
cut n (x:xs) = x : cut (n - 1) xs 
