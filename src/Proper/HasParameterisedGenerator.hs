module Proper.HasParameterisedGenerator (
  HasParameterisedGenerator(..),
  ) where
import Hedgehog (PropertyT,Property,Group(..),property,(===),forAll)
import qualified Hedgehog.Gen as Gen
import Data.String (fromString)
import Proper.HasLogicalModel
import Proper.LogicalModel
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Proxy (Proxy)

class (HasLogicalModel p m, Show m) => HasParameterisedGenerator p m where
  parameterisedGenerator :: forall t . Monad t => Set p -> PropertyT t m
  --TODO this results in lots of calls to the SAT solver during generation
  --we should maintain a cache of the results to speed things up
  genSatisfying :: forall t . Monad t => Formula p -> PropertyT t m
  genSatisfying f = do
    s <- forAll $ Gen.element (enumerateScenariosWhere f)
    parameterisedGenerator s
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

