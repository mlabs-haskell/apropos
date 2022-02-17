module Proper.HasProperties
  ( HasProperties(..)
  ) where
import Proper.LogicalModel
import Data.Set (Set)
import qualified Data.Set as Set

class (LogicalModel p) => HasProperties a p where
  satisfiesProperty :: a -> p -> Bool
  properties :: a -> Set p
  properties x = Set.fromList $ filter (satisfiesProperty x) [minBound .. maxBound]


