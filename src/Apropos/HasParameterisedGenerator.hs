module Apropos.HasParameterisedGenerator (
  HasParameterisedGenerator (..),
  runGeneratorTest,
  runGeneratorTestsWhere,
  genSatisfying,
) where

import Apropos.Gen
import Apropos.HasLogicalModel
import Apropos.LogicalModel
import Data.Proxy (Proxy)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Hedgehog (Group (..), Property, property, (===))
import Data.Proxy (Proxy (..))

class (HasLogicalModel p m, Show m) => HasParameterisedGenerator p m where
  parameterisedGenerator :: Set p -> Gen m m
  rootRetryLimit :: Proxy (m,p) -> Int
  rootRetryLimit _ = 100

--TODO caching calls to the solver in genSatisfying would probably be worth it
runGeneratorTest :: forall p m . HasParameterisedGenerator p m
                 => Proxy m -> Set p -> Property
runGeneratorTest _ s = property $ do
  (m :: m) <- handleRootRetries numRetries $ genRoot $ parameterisedGenerator s
  properties m === s
    where
      numRetries :: Int
      numRetries = rootRetryLimit (Proxy :: Proxy (m,p))

runGeneratorTestsWhere :: HasParameterisedGenerator p m
                       => Proxy m -> String -> Formula p -> Group
runGeneratorTestsWhere proxy name condition =
  Group (fromString name) $
    [ (fromString $ show $ Set.toList scenario, runGeneratorTest proxy scenario)
    | scenario <- enumerateScenariosWhere condition
    ]

genSatisfying :: HasParameterisedGenerator p m => Formula p -> Gen' m
genSatisfying f = do
  label $ fromString $ show f
  s <- element (enumerateScenariosWhere f)
  liftGen $ parameterisedGenerator s

