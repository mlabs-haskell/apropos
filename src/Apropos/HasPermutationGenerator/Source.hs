module Apropos.HasPermutationGenerator.Source (
  Source (..),
  wrapSourceWithCheck,
) where

import Apropos.Gen
import Apropos.HasLogicalModel (HasLogicalModel (properties))
import Apropos.LogicalModel (Formula)
import Control.Monad (unless)
import Data.Set (Set)

data Source p m = Source
  { sourceName :: String
  , covers :: Formula p
  , pgen :: Set p -> Gen m
  }

wrapSourceWithCheck :: forall p m. HasLogicalModel p m => Source p m -> Source p m
wrapSourceWithCheck s = s {pgen = wraped}
  where
    wraped :: Set p -> Gen m
    wraped ps = do
      label $ sourceName s
      m <- pgen s ps
      unless (ps == properties m) $
        failWithFootnote $
          "source produced incorect model "
            ++ "\nasked for:"
            ++ show ps
            ++ "\ngot:"
            ++ show (properties m :: Set p)
      pure m
