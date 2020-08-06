{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Internal.Integration.Linear.From where

import Data.List
import Internal.Matrix
import Internal.Quasi.Matrix.Quasi
import Linear (V1 (..), V2 (..), V3 (..), V4 (..))

class FromLinear m where
  type Q m :: *
  fromLinear :: m -> Q m

{- GENERATED CODE -}

instance FromLinear (V1 (V1 a)) where
  type Q (V1 (V1 a)) = Matrix 1 1 a
  fromLinear (V1 (V1 a1)) =
    [matrix| 
     a1
    |]

instance FromLinear (V1 (V2 a)) where
  type Q (V1 (V2 a)) = Matrix 1 2 a
  fromLinear (V1 (V2 a2 a3)) =
    [matrix| 
     a2  a3
    |]

instance FromLinear (V1 (V3 a)) where
  type Q (V1 (V3 a)) = Matrix 1 3 a
  fromLinear (V1 (V3 a3 a4 a5)) =
    [matrix| 
     a3  a4  a5
    |]

instance FromLinear (V1 (V4 a)) where
  type Q (V1 (V4 a)) = Matrix 1 4 a
  fromLinear (V1 (V4 a4 a5 a6 a7)) =
    [matrix| 
     a4  a5  a6  a7
    |]

instance FromLinear (V2 (V1 a)) where
  type Q (V2 (V1 a)) = Matrix 2 1 a
  fromLinear (V2 (V1 a1) (V1 a2)) =
    [matrix| 
     a1;
     a2
    |]

instance FromLinear (V2 (V2 a)) where
  type Q (V2 (V2 a)) = Matrix 2 2 a
  fromLinear (V2 (V2 a2 a3) (V2 a4 a5)) =
    [matrix| 
     a2  a3;
     a4  a5
    |]

instance FromLinear (V2 (V3 a)) where
  type Q (V2 (V3 a)) = Matrix 2 3 a
  fromLinear (V2 (V3 a3 a4 a5) (V3 a6 a7 a8)) =
    [matrix| 
     a3  a4  a5;
     a6  a7  a8
    |]

instance FromLinear (V2 (V4 a)) where
  type Q (V2 (V4 a)) = Matrix 2 4 a
  fromLinear (V2 (V4 a4 a5 a6 a7) (V4 a8 a9 a10 a11)) =
    [matrix| 
     a4  a5  a6  a7;
     a8  a9  a10  a11
    |]

instance FromLinear (V3 (V1 a)) where
  type Q (V3 (V1 a)) = Matrix 3 1 a
  fromLinear (V3 (V1 a1) (V1 a2) (V1 a3)) =
    [matrix| 
     a1;
     a2;
     a3
    |]

instance FromLinear (V3 (V2 a)) where
  type Q (V3 (V2 a)) = Matrix 3 2 a
  fromLinear (V3 (V2 a2 a3) (V2 a4 a5) (V2 a6 a7)) =
    [matrix| 
     a2  a3;
     a4  a5;
     a6  a7
    |]

instance FromLinear (V3 (V3 a)) where
  type Q (V3 (V3 a)) = Matrix 3 3 a
  fromLinear (V3 (V3 a3 a4 a5) (V3 a6 a7 a8) (V3 a9 a10 a11)) =
    [matrix| 
     a3  a4  a5;
     a6  a7  a8;
     a9  a10  a11
    |]

instance FromLinear (V3 (V4 a)) where
  type Q (V3 (V4 a)) = Matrix 3 4 a
  fromLinear (V3 (V4 a4 a5 a6 a7) (V4 a8 a9 a10 a11) (V4 a12 a13 a14 a15)) =
    [matrix| 
     a4  a5  a6  a7;
     a8  a9  a10  a11;
     a12  a13  a14  a15
    |]

instance FromLinear (V4 (V1 a)) where
  type Q (V4 (V1 a)) = Matrix 4 1 a
  fromLinear (V4 (V1 a1) (V1 a2) (V1 a3) (V1 a4)) =
    [matrix| 
     a1;
     a2;
     a3;
     a4
    |]

instance FromLinear (V4 (V2 a)) where
  type Q (V4 (V2 a)) = Matrix 4 2 a
  fromLinear (V4 (V2 a2 a3) (V2 a4 a5) (V2 a6 a7) (V2 a8 a9)) =
    [matrix| 
     a2  a3;
     a4  a5;
     a6  a7;
     a8  a9
    |]

instance FromLinear (V4 (V3 a)) where
  type Q (V4 (V3 a)) = Matrix 4 3 a
  fromLinear (V4 (V3 a3 a4 a5) (V3 a6 a7 a8) (V3 a9 a10 a11) (V3 a12 a13 a14)) =
    [matrix| 
     a3  a4  a5;
     a6  a7  a8;
     a9  a10  a11;
     a12  a13  a14
    |]

instance FromLinear (V4 (V4 a)) where
  type Q (V4 (V4 a)) = Matrix 4 4 a
  fromLinear (V4 (V4 a4 a5 a6 a7) (V4 a8 a9 a10 a11) (V4 a12 a13 a14 a15) (V4 a16 a17 a18 a19)) =
    [matrix| 
     a4  a5  a6  a7;
     a8  a9  a10  a11;
     a12  a13  a14  a15;
     a16  a17  a18  a19
    |]
