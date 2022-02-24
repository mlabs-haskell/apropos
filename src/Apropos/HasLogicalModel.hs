module Apropos.HasLogicalModel (
  HasLogicalModel (..),
) where

import Apropos.LogicalModel
import Data.Set (Set)
import qualified Data.Set as Set

class (LogicalModel p) => HasLogicalModel p m where
  satisfiesProperty :: p -> m -> Bool
  satisfiesAny :: [p] -> m -> Bool
  satisfiesAny ps m = any id (((flip satisfiesProperty) m) <$> ps)
  satisfiesAll :: [p] -> m -> Bool
  satisfiesAll ps m = all id (((flip satisfiesProperty) m) <$> ps)
  satisfiesExpression :: Formula p -> m -> Bool
  satisfiesExpression f m = satisfiesFormula f (properties m)
  properties :: m -> Set p
  properties x = Set.fromList $ filter ((flip satisfiesProperty) x) enumerated
