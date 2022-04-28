module Apropos.Pure (HasPureRunner (..)) where

import Apropos.Gen.BacktrackingTraversal
import Apropos.Gen.Enumerate
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.LogicalModel
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Hedgehog (Group (..), Property, TestLimit, property, withTests, (===))

class (HasLogicalModel p m, HasParameterisedGenerator p m) => HasPureRunner p m where
  expect :: Formula p
  script :: m -> Bool

  runPureTest :: Set p -> Property
  runPureTest s = property $ do
    (m :: m) <- traversalContainRetry numRetries $ parameterisedGenerator s
    satisfiesFormula expect s === script @p m
    where
      numRetries :: Int
      numRetries = rootRetryLimit @p

  runPureTestsWhere :: String -> Formula p -> Group
  runPureTestsWhere name condition =
    Group (fromString name) $
      [ ( fromString $ show $ Set.toList scenario
        , runPureTest scenario
        )
      | scenario <- enumerateScenariosWhere condition
      ]

  enumeratePureTest :: Set p -> Property
  enumeratePureTest s = withTests (1 :: TestLimit) $
    property $ do
      let (ms :: [m]) = enumerate $ traversalAsGen $ parameterisedGenerator s
          run m = satisfiesFormula expect s === script @p m
      sequence_ (run <$> ms)

  enumeratePureTestsWhere :: String -> Formula p -> Group
  enumeratePureTestsWhere name condition =
    Group (fromString name) $
      [ ( fromString $ show $ Set.toList scenario
        , enumeratePureTest scenario
        )
      | scenario <- enumerateScenariosWhere condition
      ]
