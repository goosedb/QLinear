{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Internal.Determinant where

import qualified Data.List as List
import Data.List.Split
import Data.Proxy
import qualified GHC.Natural as Natural
import GHC.TypeNats
import Internal.Matrix
import QLinear.Index

-- | Determinant of matrix
--
-- >>> det [matrix| 1 0; 0 1|]
-- 1
-- >>> det [matrix| 1 3; 4 2|]
-- -10
det :: Num a => Matrix n n a -> a
det (Matrix (1, _) [[a]]) = a
det (Matrix (2, _) [[a, b], [c, d]]) = a * d - b * c
det (Matrix (3, _) [[a, b, c], [d, e, f], [g, h, i]]) =
  a * (e * i - f * h) - d * (b * i - c * h) + g * (b * f - c * e)
det (Matrix (n, _) matrix) = sum $ map calcElem indices
  where
    calcElem index@(i, j) = ((!! (j - 1)) $ matrix !! (i - 1)) * unsafeAlgebraicComplement matrix n index
    indices = zip (repeat 1) [1 .. n]

-- | Typesafe algebraic complement
--
-- To use it you have to know i and j at compile time
--
-- >>> algebraicComplement [matrix| 1 2; 3 4 |] (Index @1 @1)
-- 4
-- >>> algebraicComplement [matrix| 1 2 3; 4 5 6; 7 8 9 |] (Index @1 @1)
-- -3
algebraicComplement ::
  forall n a i j.
  (KnownNat i, KnownNat j, KnownNat n, Num a, i <= n, j <= n) =>
  Matrix n n a ->
  Index i j ->
  a
algebraicComplement (Matrix (n, _) matrix) _ = unsafeAlgebraicComplement matrix n (i, j)
  where
    i = (Natural.naturalToInt $ natVal $ Proxy @i)
    j = (Natural.naturalToInt $ natVal $ Proxy @j)

-- | Algebraic complement.
--
-- Use it if you don't know indices at compile time
--
-- >>> algebraicComplement' [matrix| 1 2; 3 4 |] (1, 1)
-- Just 4
--
-- >>> algebraicComplement' [matrix| 1 2; 3 4 |] (34, 43)
-- Nothing
--
-- >>> algebraicComplement' [matrix| 1 2 3; 4 5 6; 7 8 9 |] (1, 1)
-- Just (-3)
algebraicComplement' :: Num a => Matrix n n a -> (Int, Int) -> Maybe a
algebraicComplement' (Matrix (n, _) matrix) ij@(i, j)
  | i <= n && j <= n = Just $ unsafeAlgebraicComplement matrix n ij
  | otherwise = Nothing

-- | Adjugate matrix
--
-- >>> adjugate [matrix| 1 2; 3 4|]
-- [4,-2]
-- [-3,1]
adjugate :: Num a => Matrix n n a -> Matrix n n a
adjugate (Matrix size@(n, _) matrix) = Matrix size $ chunksOf n $ adj
  where
    adj = map (unsafeAlgebraicComplement matrix n) [(i, j) | j <- [1 .. n], i <- [1 .. n]]

unsafeAlgebraicComplement :: forall a. Num a => [[a]] -> Int -> (Int, Int) -> a
unsafeAlgebraicComplement matrix n (i, j) = k * det (Matrix (n - 1, n - 1) minor)
  where
    minor = cross matrix (i - 1, j - 1)
    k = fromIntegral $ (-1) ^ (j + i)

cross :: [[a]] -> (Int, Int) -> [[a]]
cross matrix (i, j) = withoutColumn
  where
    withoutLine = cut i matrix
    withoutColumn = map (cut j) withoutLine

cut' :: Int -> [a] -> (a, [a])
cut' 0 (x : xs) = (x, xs)
cut' n (x : xs) = (x :) <$> cut' (n - 1) xs

cut :: Int -> [a] -> [a]
cut 0 (x : xs) = xs
cut n (x : xs) = x : cut (n - 1) xs
