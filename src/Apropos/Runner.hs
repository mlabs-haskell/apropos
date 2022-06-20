module Apropos.Runner (
  AproposTest(..),
  runTests,
  runTestsWhere,
) where

import Apropos.Description (DeepHasDatatypeInfo, Description (..), VariableRep, variablesToDescription)
import Apropos.Logic (Formula (..), enumerateScenariosWhere, satisfiesFormula)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Either (isRight)
import Data.String (fromString)
import Hedgehog (Group (..), Property, PropertyT, forAll, property, (===))
import Hedgehog.Internal.Property (PropertyT (PropertyT), TestT (TestT, unTest), unPropertyT)

data AproposTest d a
  = AproposTest
  { expect :: Formula (VariableRep d) 
  , test :: a -> PropertyT IO ()
  }

runTest :: forall d a. (Description d a, DeepHasDatatypeInfo d, Show a) => AproposTest d a -> d -> Property
runTest atest d = property $ do
  a <- forAll $ genForDescription d
  b <- passes (test atest a)
  b === sat
  where
    sat :: Bool
    sat = satisfiesFormula (expect atest) d

    passes :: PropertyT IO () -> PropertyT IO Bool
    passes =
      PropertyT
        . TestT
        . ExceptT
        . fmap (Right . isRight)
        . runExceptT
        . unTest
        . unPropertyT

runTests :: forall d a. (Show d, Show a, Description d a, DeepHasDatatypeInfo d) => String -> AproposTest d a ->  Group
runTests name = runTestsWhere name Yes

runTestsWhere :: forall d a. (Show d, Show a, Description d a, DeepHasDatatypeInfo d) =>  String -> Formula (VariableRep d) -> AproposTest d a -> Group
runTestsWhere name condition atest =
  Group (fromString name) $
    [ ( fromString $ show $ variablesToDescription scenario
      , runTest atest (variablesToDescription scenario)
      )
    | scenario <- enumerateScenariosWhere condition
    ]
