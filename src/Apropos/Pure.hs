module Apropos.Pure (
  PureRunner (..),
  runPureTest,
  runPureTestsWhere,
  enumeratePureTest,
  enumeratePureTestsWhere,
) where

import Apropos.Gen (errorHandler, forAll, runGenModifiable, (===))
import Apropos.Gen.Enumerate
import Apropos.Logic (Formula (..), enumerateScenariosWhere, satisfiesFormula)
import Data.String (fromString)
import Hedgehog (Group (..), Property, TestLimit, property, withTests)
import Apropos.Description (VariableRep, Description (..), variablesToDescription, DeepHasDatatypeInfo)

data PureRunner p m = PureRunner
  { expect :: Formula (VariableRep p)
  , script :: m -> Bool
  }

runPureTest :: forall d a. (Description d a, DeepHasDatatypeInfo d) => PureRunner d a -> d -> Property
runPureTest runner s = property $ runGenModifiable test >>= errorHandler
  where
    test = forAll $ do
      (m :: m) <- descriptionGen s
      satisfiesFormula (expect runner) s === script runner m

runPureTestsWhere :: forall d a. (Show d, Description d a, DeepHasDatatypeInfo d) => PureRunner d a -> String -> Formula (VariableRep d) -> Group
runPureTestsWhere runner name condition =
  Group (fromString name) $
    [ ( fromString $ show $ variablesToDescription scenario
      , runPureTest runner (variablesToDescription scenario)
      )
    | scenario <- enumerateScenariosWhere condition
    ]

enumeratePureTest :: forall d a. (Description d a, DeepHasDatatypeInfo d) => PureRunner d a -> d -> Property
enumeratePureTest runner s = withTests (1 :: TestLimit) $ property $ runGenModifiable test >>= errorHandler
  where
    test = forAll $ do
      let ms = enumerate $ descriptionGen s
          run m = satisfiesFormula (expect runner) s === script runner m
      sequence_ (run <$> ms)

enumeratePureTestsWhere :: forall d a. (Description d a, DeepHasDatatypeInfo d, Show d) => PureRunner d a -> String -> Formula (VariableRep d) -> Group
enumeratePureTestsWhere runner name condition =
  Group (fromString name) $
    [ ( fromString $ show $ variablesToDescription scenario
      , enumeratePureTest runner (variablesToDescription scenario)
      )
    | scenario <- enumerateScenariosWhere condition
    ]
