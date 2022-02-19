module Proper.HasLogicalModel
  ( HasLogicalModel(..)
  ) where
import Proper.LogicalModel
import Data.Set (Set)
import qualified Data.Set as Set

class (LogicalModel p) => HasLogicalModel p m where
  satisfiesProperty :: p -> m -> Bool
  satisfiesExpression :: Formula p -> m -> Bool
  satisfiesExpression f m = satisfiesFormula f (properties m)
  properties :: m -> Set p
  properties x = Set.fromList $ filter ((flip satisfiesProperty) x) enumerated


