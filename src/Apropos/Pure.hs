module Apropos.Pure (
  PureRunner (..),
  runPureTest,
  runPureTestsWhere,
  exhaustiveRunPureTest,
  exhaustiveRunPureTestsWhere,
) where

import Apropos.Description (DeepHasDatatypeInfo, Description (..), VariableRep, variablesToDescription)
import Apropos.Gen ((===))
import Apropos.Gen.Enumerate
import Apropos.Logic (Formula (..), enumerateScenariosWhere, satisfiesFormula)
import Data.String (fromString)
import Hedgehog (Group (..), Property)

data PureRunner p m = PureRunner
  { expect :: Formula (VariableRep p)
  , script :: m -> Bool
  }

runPureTest' :: forall d a. (Description d a, DeepHasDatatypeInfo d) => Exhaustivity -> PureRunner d a -> d -> Property
runPureTest' ex runner s =
  runTest
    ex
    (descriptionGen s)
    (\m -> satisfiesFormula (expect runner) s === script runner m)

runPureTestsWhere' :: forall d a. (Show d, Description d a, DeepHasDatatypeInfo d) => Exhaustivity -> PureRunner d a -> String -> Formula (VariableRep d) -> Group
runPureTestsWhere' ex runner name condition =
  Group (fromString name) $
    [ ( fromString $ show $ variablesToDescription scenario
      , runPureTest' ex runner (variablesToDescription scenario)
      )
    | scenario <- enumerateScenariosWhere condition
    ]

runPureTest :: forall d a. (Description d a, DeepHasDatatypeInfo d) => PureRunner d a -> d -> Property
runPureTest = runPureTest' Probablistic

runPureTestsWhere :: forall d a. (Show d, Description d a, DeepHasDatatypeInfo d) => PureRunner d a -> String -> Formula (VariableRep d) -> Group
runPureTestsWhere = runPureTestsWhere' Probablistic

exhaustiveRunPureTest :: forall d a. (Description d a, DeepHasDatatypeInfo d) => PureRunner d a -> d -> Property
exhaustiveRunPureTest = runPureTest' Exhaustive

exhaustiveRunPureTestsWhere :: forall d a. (Show d, Description d a, DeepHasDatatypeInfo d) => PureRunner d a -> String -> Formula (VariableRep d) -> Group
exhaustiveRunPureTestsWhere = runPureTestsWhere' Exhaustive
