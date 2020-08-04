module Internal.Operations where

import Internal.Matrix

import Data.Tuple
import qualified Data.List as List
import Data.List.Split (chunksOf)

(!+!) :: Num a => Matrix m n a -> Matrix m n a -> Matrix m n a
(!+!) = zipMatrixWith (+)

(*!) :: Num a => a -> Matrix m n a -> Matrix m n a
(*!) n = fmap (n *)

(!+) :: Num a => a -> Matrix m n a -> Matrix m n a
(!+) n = fmap (+ n) 

(!-!) :: Num a => Matrix m n a -> Matrix m n a -> Matrix m n a
(!-!) = zipMatrixWith (-)

(!*!) :: Num a => Matrix m n a -> Matrix n k a -> Matrix m k a
(!*!) = mulMatrixWith (*) sum

mulMatrixWith :: (a -> b -> c) -> ([c] -> d) -> Matrix m n a -> Matrix n k b -> Matrix m k d
mulMatrixWith mul add (Matrix (m, _) left) (Matrix (_, k) right) =
  Matrix (m, k) $
  chunksOf k [add $ zipWith mul line column | line <- left, column <- List.transpose right]

zipMatrixWith :: (a -> b -> c) -> Matrix m n a -> Matrix m n b -> Matrix m n c
zipMatrixWith op (Matrix size l) (Matrix _ r) =
  Matrix size $ zipWith (zipWith op) l r

transpose :: Matrix m n a -> Matrix n m a
transpose (Matrix size matrix) = Matrix (swap size) (List.transpose matrix)

neg :: Num a => Matrix m n a -> Matrix m n a
neg = ((-1) *!) 

length :: (Real a, Floating b) => Vector n a -> b
length (Matrix _ matrix) = sqrt $ sum $ squares where
  toFloating = realToFrac :: (Real a, Floating b) => a -> b
  squares = map ((**2) . toFloating) $ concat matrix
  