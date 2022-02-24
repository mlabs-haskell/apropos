module Apropos.HasParameterisedGenerator (
  HasParameterisedGenerator (..),
) where

import Apropos.Gen
import Apropos.HasLogicalModel
import Apropos.LogicalModel
import Data.Proxy (Proxy)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Hedgehog (Group (..), Property, forAll, label, property, (===))
import qualified Hedgehog.Gen as Gen

class (HasLogicalModel p m, Show m) => HasParameterisedGenerator p m where
  parameterisedGenerator :: Set p -> PGen m

  --TODO caching calls to the solver in genSatisfying would probably be worth it
  genSatisfying :: Formula p -> PGen m
  genSatisfying f = do
    label $ fromString $ show f
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
