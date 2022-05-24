module Apropos.HasPermutationGenerator.Source (
  Source (..),
  wrapSourceWithCheck,
) where

import Apropos.Gen
import Apropos.Logic (Formula, Strategy (variablesSet), satisfiesExpression)
import Control.Monad (unless)
import Data.Set (Set)

data Source p m = Source
  { sourceName :: String
  , covers :: Formula p
  , gen :: Gen m
  }

wrapSourceWithCheck :: forall p m. (Show m, Ord p, Show p, Strategy p m) => Source p m -> Source p m
wrapSourceWithCheck s = s {gen = wraped}
  where
    wraped :: Gen m
    wraped = do
      label $ sourceName s
      m <- gen s
      unless (satisfiesExpression (covers s) m) $
        failWithFootnote $
          "source produced incorect model "
            ++ "\nmodel was:"
            ++ show m
            ++ "\nproperties were:"
            ++ show (variablesSet m :: Set p)
            ++ "\ncover was:"
            ++ show (covers s)
      pure m
