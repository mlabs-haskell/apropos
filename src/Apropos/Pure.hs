module Apropos.Pure (HasPureRunner (..)) where

import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.LogicalModel
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Hedgehog (Group (..), Property, property, (===))

class (HasLogicalModel p m, HasParameterisedGenerator p m) => HasPureRunner p m where
  expect :: Proxy m -> Formula p
  script :: Proxy p -> m -> Bool

  runPureTest :: Proxy m -> Set p -> Property
  runPureTest pm s = property $ do
    (m :: m) <- parameterisedGenerator s
    let expect' = expect pm
        script' = script (Proxy :: Proxy p)
    satisfiesFormula expect' s === script' m
  runPureTestsWhere :: Proxy m -> String -> Formula p -> Group
  runPureTestsWhere pm name condition =
    Group (fromString name) $
      [ ( fromString $ show $ Set.toList scenario
        , runPureTest pm scenario
        )
      | scenario <- enumerateScenariosWhere condition
      ]
