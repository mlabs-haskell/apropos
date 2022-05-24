module Apropos.Pure (
  PureRunner (..),
  runPureTest,
  runPureTestsWhere,
  enumeratePureTest,
  enumeratePureTestsWhere,
) where

import Apropos.Gen (errorHandler, forAll, runGenModifiable, (===))
import Apropos.Gen.Enumerate
import Apropos.HasParameterisedGenerator
import Apropos.Logic (Formula (..), enumerateScenariosWhere, satisfiesFormula)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Hedgehog (Group (..), Property, TestLimit, property, withTests)

data PureRunner p m = PureRunner
  { expect :: Formula p
  , script :: m -> Bool
  }

runPureTest :: forall p m. (Ord p, HasParameterisedGenerator p m) => PureRunner p m -> Set p -> Property
runPureTest runner s = property $ runGenModifiable test >>= errorHandler
  where
    test = forAll $ do
      (m :: m) <- parameterisedGenerator s
      satisfiesFormula (expect runner) s === script runner m

runPureTestsWhere :: forall p m. (Show p, Ord p, HasParameterisedGenerator p m) => PureRunner p m -> String -> Formula p -> Group
runPureTestsWhere runner name condition =
  Group (fromString name) $
    [ ( fromString $ show $ Set.toList scenario
      , runPureTest runner scenario
      )
    | scenario <- enumerateScenariosWhere condition
    ]

enumeratePureTest :: forall p m. (Ord p, HasParameterisedGenerator p m) => PureRunner p m -> Set p -> Property
enumeratePureTest runner s = withTests (1 :: TestLimit) $ property $ runGenModifiable test >>= errorHandler
  where
    test = forAll $ do
      let (ms :: [m]) = enumerate $ parameterisedGenerator s
          run m = satisfiesFormula (expect runner) s === script runner m
      sequence_ (run <$> ms)

enumeratePureTestsWhere :: forall p m. (Show p, Ord p, HasParameterisedGenerator p m) => PureRunner p m -> String -> Formula p -> Group
enumeratePureTestsWhere runner name condition =
  Group (fromString name) $
    [ ( fromString $ show $ Set.toList scenario
      , enumeratePureTest runner scenario
      )
    | scenario <- enumerateScenariosWhere condition
    ]
