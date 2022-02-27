module Apropos.Pure (HasPureRunner (..)) where
import Apropos.Type
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.LogicalModel
import Apropos.Gen
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Hedgehog (Group (..), Property, property, (===))

class (HasLogicalModel p m, HasParameterisedGenerator p m) => HasPureRunner p m where
  expect :: m :+ p -> Formula p
  script :: m :+ p -> (m -> Bool)

  runPureTest :: m :+ p -> Set p -> Property
  runPureTest apropos s = property $ do
    (m :: m) <- handleRootRetries numRetries $ gen $ parameterisedGenerator s
    satisfiesFormula (expect apropos) s === (script apropos) m
    where
      numRetries :: Int
      numRetries = rootRetryLimit (Apropos :: m :+ p)

  runPureTestsWhere :: m :+ p -> String -> Formula p -> Group
  runPureTestsWhere pm name condition =
    Group (fromString name) $
      [ ( fromString $ show $ Set.toList scenario
        , runPureTest pm scenario
        )
      | scenario <- enumerateScenariosWhere condition
      ]

