module Proper.HasParameterisedGenerator (
    HasParameterisedGenerator(..)
  ) where
import Hedgehog (PropertyT,Property,Group(..),property,(===))
import Data.String (fromString)
import Proper.HasProperties
import Proper.LogicalModel
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Proxy (Proxy)
import SAT.MiniSat (Formula)

class (HasProperties m p, Show m) => HasParameterisedGenerator m p where
  parameterisedGenerator :: forall t . Monad t => Set p -> PropertyT t m
  runGeneratorTest :: Proxy m -> Set p -> Property
  runGeneratorTest _ s = property $ do
    (m :: m) <- parameterisedGenerator s
    properties m === s
  runGeneratorTestsWhere :: Proxy m -> String -> Formula p -> Group
  runGeneratorTestsWhere proxy name condition =
    Group (fromString name) $
      [ (fromString $ show $ Set.toList scenario, runGeneratorTest proxy scenario)
      | scenario <- enumerateScenariosWhere condition
      ]

