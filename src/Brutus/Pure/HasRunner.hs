module Brutus.Pure.HasRunner ( HasRunner(..) ) where
import Brutus.HasLogicalModel
import Brutus.HasParameterisedGenerator
import Brutus.LogicalModel
import Hedgehog (Property,Group(..),property,(===))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Proxy (Proxy(..))


class (HasLogicalModel p m, HasParameterisedGenerator p m) => HasRunner p m where
  expect :: Proxy m -> Formula p
  script :: Proxy p -> m -> Bool

  runPureTest :: Proxy m -> Set p -> Property
  runPureTest pm s = property $ do
    (m :: m) <- parameterisedGenerator s
    let expect' = expect pm
        script'    = script (Proxy :: Proxy p)
    satisfiesFormula expect' s === script' m
  runPureTestsWhere :: Proxy m -> String -> Formula p -> Group
  runPureTestsWhere pm name condition =
    Group (fromString name) $
      [ ( fromString $ show $ Set.toList scenario
        , runPureTest pm scenario)
      | scenario <- enumerateScenariosWhere condition
      ]

