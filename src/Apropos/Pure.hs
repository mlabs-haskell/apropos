module Apropos.Pure (HasPureRunner (..)) where

import Apropos.Gen (errorHandler, forAll, runGenModifiable, (===))
import Apropos.Gen.Enumerate
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.LogicalModel
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Hedgehog (Group (..), Property, TestLimit, property, withTests)

class (HasLogicalModel p m, HasParameterisedGenerator p m) => HasPureRunner p m where
  expect :: Formula p
  script :: m -> Bool

  runPureTest :: Set p -> Property
  runPureTest s = property $ runGenModifiable test >>= errorHandler
    where
      test = forAll $ do
        (m :: m) <- parameterisedGenerator s
        satisfiesFormula (expect @p) s === script @p m

  runPureTestsWhere :: String -> Formula p -> Group
  runPureTestsWhere name condition =
    Group (fromString name) $
      [ ( fromString $ show $ Set.toList scenario
        , runPureTest scenario
        )
      | scenario <- enumerateScenariosWhere condition
      ]

  enumeratePureTest :: Set p -> Property
  enumeratePureTest s = withTests (1 :: TestLimit) $ property $ runGenModifiable test >>= errorHandler
    where
      test = forAll $ do
        let (ms :: [m]) = enumerate $ parameterisedGenerator s
            run m = satisfiesFormula (expect @p) s === script @p m
        sequence_ (run <$> ms)

  enumeratePureTestsWhere :: String -> Formula p -> Group
  enumeratePureTestsWhere name condition =
    Group (fromString name) $
      [ ( fromString $ show $ Set.toList scenario
        , enumeratePureTest scenario
        )
      | scenario <- enumerateScenariosWhere condition
      ]
