{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module QLinear.Identity (e, Identity, HasIdentity (..)) where

import Data.Proxy
import GHC.TypeNats
import Internal.Matrix

type Identity n a = Matrix n n a

class HasIdentity a where
  zero :: a
  one :: a

instance (Num a) => HasIdentity a where
  zero = 0
  one = 1

-- | Polymirphic identity matrix
--
-- Identity matrix can udjust to other matrix with known size. If size is unknown, just set it yourself
--
-- >>> e :: Identity 4 Int
-- [1,0,0,0]
-- [0,1,0,0]
-- [0,0,1,0]
-- [0,0,0,1]
-- >>> e ~+~ [matrix| 1 2; 3 4 |]
-- [2,2]
-- [3,5]
e :: forall n a. (KnownNat n, HasIdentity a) => Identity n a
e = Matrix (n, n) $ finiteIdentityList (n, n) one zero
  where
    n = fromIntegral $ natVal (Proxy @n)

infiniteIdentityList :: a -> a -> [[a]]
infiniteIdentityList o z = stream (o : repeat z)
  where
    stream seed = seed : stream (z : seed)

finiteIdentityList :: (Int, Int) -> a -> a -> [[a]]
finiteIdentityList (m, n) o z = map (take n) $ take m $ infiniteIdentityList o z
