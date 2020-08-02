module Internal.Determinant where

import Internal.Matrix

class Determinant n where
  det :: Num a => Matrix n n a -> a

instance Determinant 1 where
  det (Matrix _ [[a]]) = a

instance Determinant 2 where
  det (Matrix _ [[a, b], [c, d]]) = a * d - b * c

instance Determinant 3 where
  det (Matrix _ [[a, b, c], [d, e, f], [g, h, k]]) =
    a * e * k + b * f * g + c * d * h - c * e * g - b * d * k - a * f * h