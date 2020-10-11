{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Internal.Quasi.Matrix.Pattern.Quote where

import Language.Haskell.TH.Syntax
import Internal.Matrix
import Data.Char

-- DRAFT. VERY DITRY AND VERY RAW DRAFT

pat :: String -> Q Pat
pat raw = pure $ SigP (ConP 'Matrix [WildP, ListP $ map (VarP . mkName) ls]) (foldl AppT (ConT ''Matrix) [sizeType $ fromIntegral $ length ls, WildCardT, WildCardT])
  where
    sizeType = LitT . NumTyLit
    ls = map trim $ split ';' raw



split :: Char -> String -> [String]
split sep = reverse . go [] [] where
  go res [] [] = res
  go res str [] = reverse str : res
  go res str (c:cs)
    | c == sep = go (reverse str: res) [] cs
    | otherwise = go res (c:str) cs

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace