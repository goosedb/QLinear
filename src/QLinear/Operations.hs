{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module QLinear.Operations
  ( length,
    mulMatricesWith,
    neg,
    transpose,
    zipMatricesWith,
    det,
    algebraicComplement,
    algebraicComplement',
    adjugate,
    inverted,
    (*~),
    (~*~),
    (~+),
    (+~),
    (~+~),
    (~-~),
  )
where

import qualified Data.List as List
import Data.List.Split (chunksOf)
import Data.Tuple
import Internal.Determinant (adjugate, algebraicComplement, algebraicComplement', det)
import Internal.Matrix
import Internal.Quasi.Matrix.Quasi
import Prelude hiding (length)

-- | Adds two matrices
--
-- >>> [matrix| 1 2 |] ~+~ [matrix| 2 3 |]
-- [3,5]
(~+~) :: Num a => Matrix m n a -> Matrix m n a -> Matrix m n a
(~+~) = zipMatricesWith (+)

infixl 6 ~+~

-- | Multuplies all elements of matrix __m__ by __k__
--
-- >>> 5 *~ [matrix| 1 2 3; 4 5 6 |]
-- [5,10,15]
-- [20,25,30]
(*~) ::
  Num a =>
  -- | k
  a ->
  -- | m
  Matrix m n a ->
  Matrix m n a
(*~) n = fmap (n *)

infixl 7 *~

-- | Adds __a__ to all elements of matrix __m__
--
-- >>> [matrix| 1 2 3 |] ~+ 8
-- [9,10,11]
(~+) ::
  Num a =>
  Matrix m n a ->
  a ->
  Matrix m n a
(~+) m n = (+ n) <$> m

infixl 6 ~+

-- | Flipped __~+__ :)
(+~) :: Num a => a -> Matrix m n a -> Matrix m n a
(+~) = flip (~+)

infixl 6 +~

-- | Substracts second matrix from first one
--
-- >>> [matrix| 1 2 3 |] ~-~ [matrix| 3 2 1 |]
-- [-2,0,2]
(~-~) :: Num a => Matrix m n a -> Matrix m n a -> Matrix m n a
(~-~) = zipMatricesWith (-)

infixl 6 ~-~


-- | Multiplies two matrix
--
-- >>> [matrix| 1 2; 3 4 |] ~*~ [matrix| 1; 2 |]
-- [5]
-- [11]
(~*~) :: Num a => Matrix m n a -> Matrix n k a -> Matrix m k a
(~*~) = mulMatricesWith (*) (+)

infixl 7 ~*~

-- | Generalized matrices multiplication
mulMatricesWith ::
  -- | operation "__*__"
  (a -> b -> c) ->
  -- | operation "__+__"
  (c -> c -> c) ->
  Matrix m n a ->
  Matrix n k b ->
  Matrix m k c
mulMatricesWith mul add (Matrix (m, _) left) (Matrix (_, k) right) =
  Matrix (m, k) $
    chunksOf k [foldl1 add $ zipWith mul line column | line <- left, column <- List.transpose right]

-- | Generalized matrices addition
zipMatricesWith ::
  -- | operation "__+__"
  (a -> b -> c) ->
  Matrix m n a ->
  Matrix m n b ->
  Matrix m n c
zipMatricesWith op (Matrix size l) (Matrix _ r) =
  Matrix size $ zipWith (zipWith op) l r

-- | Transposes matrix
--
-- >>> transpose [matrix| 1 2 3; 4 5 6 |]
-- [1,4]
-- [2,5]
-- [3,6]
transpose :: Matrix m n a -> Matrix n m a
transpose (Matrix size matrix) = Matrix (swap size) (List.transpose matrix)

-- | Nagates all elements of matrix
--
-- >>> neg [matrix| 1 2 3 |]
-- [-1,-2,-3]
neg :: Num a => Matrix m n a -> Matrix m n a
neg = ((-1) *~)

-- | Length of vector
--
-- >>> length [vector| 3 4 |]
-- 5.0
-- >>> length [vector| 1 1 |]
-- 1.4142135623730951
length :: forall a b n. (Real a, Floating b) => Vector n a -> b
length (Matrix _ matrix) = sqrt $ sum $ squares
  where
    toFloating = realToFrac :: a -> b
    squares = map ((** 2) . toFloating) $ concat matrix

-- | Inverted matrix
--
-- >>> inverted [matrix| 1 2; 3 4|]
-- Just [-2.0,1.0]
--      [1.5,-0.5]
-- >>> inverted [matrix| 1 4; 1 4|]
-- Nothing
inverted :: forall a b n. (Fractional b, Eq a, Real a) => Matrix n n a -> Maybe (Matrix n n b)
inverted (Matrix size@(1, 1) [[a]]) = if a /= 0 then Just (Matrix size [[1.0 / toFloating a]]) else Nothing
  where
    toFloating = realToFrac :: a -> b
inverted matrix = if determinant /= 0 then Just $ ((invertedDet *) . toFloating) <$> adj else Nothing
  where
    toFloating = realToFrac :: a -> b
    determinant = det matrix
    invertedDet = 1.0 / toFloating determinant
    adj = adjugate matrix
