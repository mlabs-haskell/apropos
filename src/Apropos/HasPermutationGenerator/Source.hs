module Apropos.HasPermutationGenerator.Source (
  Source (..),
  wrapSourceWithCheck,
) where

import Apropos.Gen
import Apropos.HasLogicalModel (HasLogicalModel (properties), satisfiesExpression)
import Apropos.LogicalModel (Formula)
import Control.Monad (unless)
import Data.Set (Set)

data Source p m = Source
  { sourceName :: String
  , covers :: Formula p
  , gen :: Gen m
  }

wrapSourceWithCheck :: forall p m. (Show m, HasLogicalModel p m) => Source p m -> Source p m
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
            ++ show (properties m :: Set p)
            ++ "\ncover was:"
            ++ show (covers s)
      pure m
