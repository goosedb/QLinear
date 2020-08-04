{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Internal.Index where

import GHC.TypeNats

data Index (i :: Nat) (j :: Nat) = Index