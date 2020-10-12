{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module QLinear.Integration.Linear.To where

import Data.Kind (Type)
import Internal.Matrix
import Linear (V1 (..), V2 (..), V3 (..), V4 (..))

class ToLinear m where
  type L m :: Type
  toLinear :: m -> L m

{- GENERATED CODE -}

instance ToLinear (Matrix 1 1 a) where
  type L (Matrix 1 1 a) = V1 (V1 a)
  toLinear (Matrix _ [[a1]]) = V1 (V1 a1)

instance ToLinear (Matrix 1 2 a) where
  type L (Matrix 1 2 a) = V1 (V2 a)
  toLinear (Matrix _ [[a2, a3]]) = V1 (V2 a2 a3)

instance ToLinear (Matrix 1 3 a) where
  type L (Matrix 1 3 a) = V1 (V3 a)
  toLinear (Matrix _ [[a3, a4, a5]]) = V1 (V3 a3 a4 a5)

instance ToLinear (Matrix 1 4 a) where
  type L (Matrix 1 4 a) = V1 (V4 a)
  toLinear (Matrix _ [[a4, a5, a6, a7]]) = V1 (V4 a4 a5 a6 a7)

instance ToLinear (Matrix 2 1 a) where
  type L (Matrix 2 1 a) = V2 (V1 a)
  toLinear (Matrix _ [[a1], [a2]]) = V2 (V1 a1) (V1 a2)

instance ToLinear (Matrix 2 2 a) where
  type L (Matrix 2 2 a) = V2 (V2 a)
  toLinear (Matrix _ [[a2, a3], [a4, a5]]) = V2 (V2 a2 a3) (V2 a4 a5)

instance ToLinear (Matrix 2 3 a) where
  type L (Matrix 2 3 a) = V2 (V3 a)
  toLinear (Matrix _ [[a3, a4, a5], [a6, a7, a8]]) = V2 (V3 a3 a4 a5) (V3 a6 a7 a8)

instance ToLinear (Matrix 2 4 a) where
  type L (Matrix 2 4 a) = V2 (V4 a)
  toLinear (Matrix _ [[a4, a5, a6, a7], [a8, a9, a10, a11]]) = V2 (V4 a4 a5 a6 a7) (V4 a8 a9 a10 a11)

instance ToLinear (Matrix 3 1 a) where
  type L (Matrix 3 1 a) = V3 (V1 a)
  toLinear (Matrix _ [[a1], [a2], [a3]]) = V3 (V1 a1) (V1 a2) (V1 a3)

instance ToLinear (Matrix 3 2 a) where
  type L (Matrix 3 2 a) = V3 (V2 a)
  toLinear (Matrix _ [[a2, a3], [a4, a5], [a6, a7]]) = V3 (V2 a2 a3) (V2 a4 a5) (V2 a6 a7)

instance ToLinear (Matrix 3 3 a) where
  type L (Matrix 3 3 a) = V3 (V3 a)
  toLinear (Matrix _ [[a3, a4, a5], [a6, a7, a8], [a9, a10, a11]]) =
    V3 (V3 a3 a4 a5) (V3 a6 a7 a8) (V3 a9 a10 a11)

instance ToLinear (Matrix 3 4 a) where
  type L (Matrix 3 4 a) = V3 (V4 a)
  toLinear (Matrix _ [[a4, a5, a6, a7], [a8, a9, a10, a11], [a12, a13, a14, a15]]) =
    V3 (V4 a4 a5 a6 a7) (V4 a8 a9 a10 a11) (V4 a12 a13 a14 a15)

instance ToLinear (Matrix 4 1 a) where
  type L (Matrix 4 1 a) = V4 (V1 a)
  toLinear (Matrix _ [[a1], [a2], [a3], [a4]]) =
    V4 (V1 a1) (V1 a2) (V1 a3) (V1 a4)

instance ToLinear (Matrix 4 2 a) where
  type L (Matrix 4 2 a) = V4 (V2 a)
  toLinear (Matrix _ [[a2, a3], [a4, a5], [a6, a7], [a8, a9]]) =
    V4 (V2 a2 a3) (V2 a4 a5) (V2 a6 a7) (V2 a8 a9)

instance ToLinear (Matrix 4 3 a) where
  type L (Matrix 4 3 a) = V4 (V3 a)
  toLinear (Matrix _ [[a3, a4, a5], [a6, a7, a8], [a9, a10, a11], [a12, a13, a14]]) =
    V4 (V3 a3 a4 a5) (V3 a6 a7 a8) (V3 a9 a10 a11) (V3 a12 a13 a14)

instance ToLinear (Matrix 4 4 a) where
  type L (Matrix 4 4 a) = V4 (V4 a)
  toLinear (Matrix _ [[a4, a5, a6, a7], [a8, a9, a10, a11], [a12, a13, a14, a15], [a16, a17, a18, a19]]) =
    V4 (V4 a4 a5 a6 a7) (V4 a8 a9 a10 a11) (V4 a12 a13 a14 a15) (V4 a16 a17 a18 a19)
