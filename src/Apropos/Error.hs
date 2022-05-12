module Apropos.Error (internalError) where

internalError :: String -> a
internalError desc = error $
  "Internal error: " ++ desc
  ++ "\n This is a bug in apropos please report it at https://github.com/mlabs-haskell/apropos/issues"
