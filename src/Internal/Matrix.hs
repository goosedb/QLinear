{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Internal.Matrix where

import qualified Data.List as List
import Data.List.Split
import Data.Proxy
import Data.Tuple
import qualified GHC.Natural as Natural
import GHC.TypeNats
import Prelude hiding (length)

data Matrix (m :: Nat) (n :: Nat) a where
  Matrix :: (Int, Int) -> [[a]] -> Matrix m n a

instance Show a => Show (Matrix m n a) where
  show (Matrix _ matrix) = List.intercalate "\n" $ map show matrix

instance Functor (Matrix m n) where
  fmap f (Matrix size a) = Matrix size $ map (map f) a

instance Applicative (Matrix m n) where
  pure = Matrix (1, 1) . pure . pure
  Matrix size fs <*> (Matrix _ as) =
    Matrix size $ map (uncurry (<*>)) $ zip fs as

instance (Eq a) => Eq (Matrix m n a) where
  Matrix _ a == Matrix _ b = a == b

