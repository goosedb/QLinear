module Internal.Quasi.Quasi where

isNotDefinedAs :: String -> String -> a
isNotDefinedAs name as = error $ "You cannot use " <> name <> " quasi as " <> as
