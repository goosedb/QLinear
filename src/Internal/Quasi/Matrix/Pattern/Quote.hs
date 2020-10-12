{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Internal.Quasi.Matrix.Pattern.Quote (pat) where

import Data.Char (isSpace)
import Language.Haskell.TH.Lib (listP, litT, numTyLit, varP)
import Language.Haskell.TH.Syntax (Pat, Q, mkName)

import Internal.Matrix (Matrix (..))

-- DRAFT. VERY DIRTY AND VERY RAW DRAFT

pat :: String -> Q Pat
pat raw = [p| (Matrix _ $ps :: Matrix $size _ _) |]
  where
    ls = map trim $ split ';' raw
    size = litT $ numTyLit $ fromIntegral $ length ls
    ps = listP $ map (varP . mkName) ls

split :: Char -> String -> [String]
split sep = reverse . go [] [] where
  go res [] [] = res
  go res str [] = reverse str : res
  go res str (c:cs)
    | c == sep = go (reverse str: res) [] cs
    | otherwise = go res (c:str) cs

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
