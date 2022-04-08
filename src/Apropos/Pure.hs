module Apropos.Pure (HasPureRunner (..)) where

import Apropos.Gen.BacktrackingTraversal
import Apropos.Gen.Enumerate
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.LogicalModel
import Apropos.Type
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Hedgehog (Group (..), Property, TestLimit, property, withTests, (===))

class (HasLogicalModel p m, HasParameterisedGenerator p m) => HasPureRunner p m where
  expect :: m :+ p -> Formula p
  script :: m :+ p -> (m -> Bool)

  runPureTest :: m :+ p -> Set p -> Property
  runPureTest apropos s = property $ do
    (m :: m) <- traversalContainRetry numRetries $ parameterisedGenerator s
    satisfiesFormula (expect apropos) s === script apropos m
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

  enumeratePureTest :: m :+ p -> Set p -> Property
  enumeratePureTest apropos s = withTests (1 :: TestLimit) $
    property $ do
      let (ms :: [m]) = enumerate $ traversalAsGen $ parameterisedGenerator s
          run m = satisfiesFormula (expect apropos) s === script apropos m
      sequence_ (run <$> ms)

  enumeratePureTestsWhere :: m :+ p -> String -> Formula p -> Group
  enumeratePureTestsWhere pm name condition =
    Group (fromString name) $
      [ ( fromString $ show $ Set.toList scenario
        , enumeratePureTest pm scenario
        )
      | scenario <- enumerateScenariosWhere condition
      ]
