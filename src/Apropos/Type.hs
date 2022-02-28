module Apropos.Type (
  Apropos (..),
  (:+),
) where

type type' :+ logic' = Apropos (type', logic')

data Apropos a = Apropos
