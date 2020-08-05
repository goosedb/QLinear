module Internal.Quasi.Quasi where

import qualified Data.List as List

isNotDefinedAs :: String -> String -> a
isNotDefinedAs name as = error $ "You cannot use " <> name <> " quasi as " <> as

unwrap :: Either [String] a -> a
unwrap (Left a) = error $ List.intercalate ", " a
unwrap (Right a ) = a
