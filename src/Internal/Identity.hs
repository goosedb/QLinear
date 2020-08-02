{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
module Internal.Identity where

import Data.Proxy
import GHC.TypeNats
import qualified GHC.Natural as Natural

import Internal.Matrix

class HasIdentity a where
  zero :: a
  one :: a

instance (Num a) => HasIdentity a where
  zero = 0
  one = 1

e :: forall n a. (KnownNat n, HasIdentity a)
  => Matrix n n a
e = Matrix (n, n) $ finiteIdentityList (n, n) one zero where
  n = Natural.naturalToInt $ natVal (Proxy @n)

infiniteIdentityList :: a -> a -> [[a]]
infiniteIdentityList o z = stream (o : repeat z)
  where
    stream seed = seed : stream (z : seed)

finiteIdentityList :: (Int, Int) -> a -> a -> [[a]]
finiteIdentityList (m, n) o z = map (take n) $ take m $ infiniteIdentityList o z