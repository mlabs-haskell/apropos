module Apropos.HasLogicalModel (
  HasLogicalModel (..),
) where

import Apropos.LogicalModel
import Data.Set (Set)
import Data.Set qualified as Set

class (LogicalModel p) => HasLogicalModel p m where
  satisfiesProperty :: p -> m -> Bool
  satisfiesAny :: [p] -> m -> Bool
  satisfiesAny ps m = or (flip satisfiesProperty m <$> ps)
  satisfiesAll :: [p] -> m -> Bool
  satisfiesAll ps m = and (flip satisfiesProperty m <$> ps)
  satisfiesExpression :: Formula p -> m -> Bool
  satisfiesExpression f m = satisfiesFormula f (properties m)
  properties :: m -> Set p
  properties x = Set.fromList $ filter (`satisfiesProperty` x) enumerated
