{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module QLinear.Index where

import Data.Proxy
import qualified GHC.Natural as Natural
import GHC.TypeNats

-- | Typesafe index. To construct it use TypeApplications
--
-- >>> Index @1 @3
-- Index 1 3
-- >>> :t Index @1 @3
-- Index @1 @3 :: Index 1 3
data Index (i :: Nat) (j :: Nat) = Index

instance forall i j. (KnownNat i, KnownNat j) => Show (Index i j) where
  show _ = "Index " <> show i <> " " <> show j
    where
      i = natVal (Proxy @i)
      j = natVal (Proxy @j)
