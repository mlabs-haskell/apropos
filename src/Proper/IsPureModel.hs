module Proper.IsPureModel ( IsPureModel(..) ) where
import Proper.HasLogicalModel
import Proper.HasParameterisedGenerator
import Proper.LogicalModel
import Hedgehog (Property,Group(..),property,(===))
import SAT.MiniSat ( Formula (..) )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Proxy (Proxy(..))


class (HasLogicalModel m p, HasParameterisedGenerator m p) => IsPureModel m p where
  expect :: Proxy m -> Formula p
  run :: Proxy p -> m -> Bool

  runPureTest :: Proxy m -> Set p -> Property
  runPureTest pm s = property $ do
    (m :: m) <- parameterisedGenerator s
    let expect' = expect pm
        run'    = run (Proxy :: Proxy p)
    satisfiesFormula expect' s === run' m
  runPureTestsWhere :: Proxy m -> String -> Formula p -> Group
  runPureTestsWhere pm name condition =
    Group (fromString name) $
      [ ( fromString $ show $ Set.toList scenario
        , runPureTest pm scenario)
      | scenario <- enumerateScenariosWhere condition
      ]

