module Main where

import Data.Typeable

import Test.Hspec

import Linear.Matrix (det22, det33, det44)
import Linear (V2(..), V3(..), V4(..))

import Internal.Quasi.Matrix.Quasi
import Internal.Quasi.Operator.Quasi
import Internal.Matrix
import Internal.Operations
import Internal.Identity
import Internal.Index
import Internal.Determinant

newtype TestMatrix m n a = TestMatrix (Matrix m n a)

instance Eq a => Eq (TestMatrix m n a) where
  TestMatrix (Matrix ls lv) == TestMatrix (Matrix rs rv) = ls == rs && lv == rv

instance Show a => Show (TestMatrix m n a) where
  show (TestMatrix (Matrix s l)) = show s <> " " <> show l

matrixEq l r = TestMatrix l `shouldBe` TestMatrix r

main :: IO ()
main = hspec do  

  describe "QuasiQuoter" do
    it "build matrix by quasi qouter" do
      {- doesn't compile  -}
      -- [matrix| 1; 2 3; 4 5 6 |] `matrixEq` undefined
      [matrix| 1 |] `matrixEq` (Matrix (1, 1) [[1]] :: Matrix 1 1 Int)
      [matrix| 1 2 |] `matrixEq` (Matrix (1, 2) [[1, 2]] :: Matrix 1 2 Int)
      [matrix| 1 2 3 |] `matrixEq` (Matrix (1, 3) [[1, 2, 3]] :: Matrix 1 3 Int)
      [matrix| 1 2 3 4|] `matrixEq` (Matrix (1, 4) [[1, 2, 3, 4]] :: Matrix 1 4 Int)
      [matrix| 1; 2; 3 |] `matrixEq` (Matrix (3, 1) [[1], [2], [3]] :: Matrix 3 1 Int)
      [matrix| (1 + 2) -4 5.0 |] `matrixEq` (Matrix (1, 3) [[3, -4, 5]] :: Matrix 1 3 Double)
      [matrix| ("foo") ("bar") |] `matrixEq` (Matrix (1, 2) [["foo", "bar"]] :: Matrix 1 2 String)
    it "build vector by quasi qouter" do
      [vector| 1 2 3 |] `matrixEq` (Matrix (3, 1) [[1], [2], [3]] :: Vector 3 Int)
    it "build operator by quasi quoter" do
      [operator| (x, y) => (x, y) |] `matrixEq` (Matrix (2, 2) [[1, 0], [0, 1]] :: Matrix 2 2 Int)
      [operator| (x, y) => (2 * x, y) |] `matrixEq` (Matrix (2, 2) [[2, 0], [0, 1]] :: Matrix 2 2 Int)
      [operator| (x, y) => (y, x) |] `matrixEq` (Matrix (2, 2) [[0, 1], [1, 0]] :: Matrix 2 2 Int)
      [operator| (x, y) => (x + 2 * y, y) |] `matrixEq` (Matrix (2, 2) [[1, 2], [0, 1]] :: Matrix 2 2 Int)

  describe "Addition" do
    it "two matrices" do
      {- doesn't compile  -}
      -- [matrix| 1 2 |] !+! [matrix| 1 2 3 |] `matrixEq` undefined
      [matrix| 1 2 |] !+! [matrix| 3 4 |] `matrixEq` [matrix| 4 6 |]
      [matrix| 1 2; 3 4 |] !+! [matrix| 3 4; 5 6 |] `matrixEq` [matrix| 4 6; 8 10 |]
    it "matrix and identity matrix" do 
      {- doesn't compile  -}
      -- [matrix| 1 2 |] !+! e `matrixEq` undefined
      [matrix| 1 2; 3 4 |] !+! e `matrixEq` [matrix| 2 2; 3 5 |]
      e !+! [matrix| 1 2; 3 4 |] `matrixEq` [matrix| 2 2; 3 5 |]
    it "two identity matrices" do
      {- doesn't compile  -}
      -- (e :: Matrix 3 3 Int) !+! (e :: Matrix 4 4 Int) `matrixEq` undefined
      (e :: Matrix 3 3 Int) !+! e `matrixEq` [matrix| 2 0 0; 0 2 0; 0 0 2 |]
    it "two vectors" do
      {- doesn't compile  -}
      -- [vector| 1 2 |] !+! [vector| 1 2 3 |] `matrixEq` undefined
      [vector| 1 2 |] !+! [vector| 3 4 |] `matrixEq` [vector| 4 6 |]

  describe "Substaction" do
    it "two matrices" do
      {- doesn't compile  -}
      -- [matrix| 1 2 |] !-! [matrix| 1 2 3 |] `matrixEq` undefined
      [matrix| 1 2 |] !-! [matrix| 3 4 |] `matrixEq` [matrix| -2 -2 |]
      [matrix| 1 2; 3 4 |] !-! [matrix| 3 4; 5 6 |] `matrixEq` [matrix| -2 -2; -2 -2 |]
    it "matrix and identity matrix" do
      {- doesn't compile  -}
      -- [matrix| 1 2 |] !-! e `matrixEq` undefined
      [matrix| 1 2; 3 4 |] !-! e `matrixEq` [matrix| 0 2; 3 3 |]
      e !-! [matrix| 1 2; 3 4 |] `matrixEq` [matrix| 0 -2; -3 -3 |]
    it "two identity matrices" do
      {- doesn't compile  -}
      -- (e :: Matrix 3 3 Int) !-! (e :: Matrix 4 4 Int) `matrixEq` undefined
      (e :: Matrix 3 3 Int) !-! e `matrixEq` [matrix| 0 0 0; 0 0 0; 0 0 0 |]
    it "two vectors" do
      {- doesn't compile  -}
      -- [vector| 1 2 |] !-! [vector| 1 2 3 |] `matrixEq` undefined
      [vector| 1 2 |] !-! [vector| 3 4 |] `matrixEq` [vector| -2 -2 |]

  describe "Multiplication" do
    it "two matrices" do 
      {- doesn't compile  -}
      -- [matrix| 1 2 |] !*! [matrix| 1 2 |] `matrixEq` undefined
      [matrix| 2 |] !*! [matrix| 3 |] `matrixEq` [matrix| 6 |]
      [matrix| 1 2 |] !*! [matrix| 1; 2 |] `matrixEq` [matrix| 5 |]
      [matrix| 1 2 |] !*! [matrix| 1 2; 3 4 |] `matrixEq` [matrix| 7 10 |]
      [matrix| 1 2; 3 4 |] !*! [matrix| 2 3; 4 5 |] `matrixEq` [matrix| 10 13; 22 29 |]
    it "matrix and identity matrix" do
      {- doesn't compile  -}
      -- [matrix| 1 2 |] !*! (e :: Matrix 3 3 Int) `matrixEq` undefined
      [matrix| 1 2 |] !*! e `matrixEq` [matrix| 1 2 |]
      [matrix| 1 2; 3 4 |] !*! e `matrixEq` [matrix| 1 2; 3 4 |]
      e !*! [matrix| 1 2; 3 4 |] `matrixEq` [matrix| 1 2; 3 4 |]
    it "two identity matrices" do
      {- doesn't compile  -}
      -- (e :: Matrix 3 3 Int) !*! (e :: Matrix 4 4 Int) `matrixEq` undefined
      (e :: Matrix 3 3 Int) !*! e `matrixEq` e

  describe "Determinant" do
    it "not identity matrix" do
      let [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p] = [1, 3, 5, 7, 8, 2, 4, 6, 9, 10, 11, 13, 15, 17, 12, 14]
      {- doesn't compile  -}
      --  det [matrix| 1 2 |] `shouldBe` undefined
      det [matrix| a |] `shouldBe` a
      det [matrix| a b; c d |] `shouldBe` det22 (V2 (V2 a b) (V2 c d))
      det [matrix| a b c; d e f; g h i |] `shouldBe` det33 (V3 (V3 a b c) (V3 d e f) (V3 g h i))
      det [matrix| 
        a b c d; 
        e f g h; 
        i j k l; 
        m n o p |] `shouldBe` det44 (V4 (V4 a b c d) (V4 e f g h) (V4 i j k l) (V4 m n o p))
      det [matrix| 
        -1  2  3  6   5; 
        10  7 14  9  18; 
        11 22 13 26  15; 
        30 17 34 19  38; 
        21 42 23 46 -25 |] `shouldBe` (-125200)
        
  describe "Identity matrix" do
    it "multiplication" do
      let m = [matrix| 1 2 3 4; 5 6 7 8 |]
      let i = e :: Matrix 3 3 Int
      (m !*! e) `matrixEq` (e !*! m)
      (i !*! e) `matrixEq` e
      (e !*! i) `matrixEq` e
    it "determinant" do
      det (e :: Matrix 1 1 Int) `shouldBe` 1
      det (e :: Matrix 2 2 Int) `shouldBe` 1
      det (e :: Matrix 3 3 Int) `shouldBe` 1
      det (e :: Matrix 4 4 Int) `shouldBe` 1
      det (e :: Matrix 5 5 Int) `shouldBe` 1
  
  describe "Algebraic complement" do
    it "typesafe" do
      {- doesn't compile -}
      -- algebraicComplement [matrix| 1 2; 3 4 |] (Index @1 @3) `shouldBe` 4
      algebraicComplement [matrix| 1 2; 3 4 |] (Index @1 @1) `shouldBe` 4
      algebraicComplement [matrix| 1 2; 3 4 |] (Index @1 @2) `shouldBe` (-6)
    it "not typesafe" do
      algebraicComplement' [matrix| 1 2; 3 4 |] (1, 3) `shouldBe` Nothing
      algebraicComplement' [matrix| 1 2; 3 4 |] (1, 1) `shouldBe` Just 4
      algebraicComplement' [matrix| 1 2; 3 4 |] (1, 2) `shouldBe` Just (-6)