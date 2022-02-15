module Proper.HasParameterisedGenerator (
    HasParameterisedGenerator(..)
  ) where
import Hedgehog (MonadGen,Property,Group(..),property,forAll,(===))
import Data.String (fromString)
import Proper.HasProperties
import Proper.Proposition
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Proxy (Proxy)
import SAT.MiniSat (Formula)

class (HasProperties m p, Show m) => HasParameterisedGenerator m p where
  parameterisedGenerator :: MonadGen g => Proxy m -> Set p -> g m
  runGeneratorTest :: Proxy m -> Set p -> Property
  runGeneratorTest proxy s = property $ do
    m <- forAll $ parameterisedGenerator proxy s
    properties m === s
  runGeneratorTestsWhere :: Proxy m -> String -> Formula p -> Group
  runGeneratorTestsWhere proxy name condition =
    Group (fromString name) $
      [ (fromString $ show $ Set.toList scenario, runGeneratorTest proxy scenario)
      | scenario <- enumerateScenariosWhere condition
      ]

