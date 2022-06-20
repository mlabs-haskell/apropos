module Apropos.Pure (
  runPureTest,
  runPureTestsWhere,
) where

import Apropos.Description (DeepHasDatatypeInfo, Description (..), VariableRep, variablesToDescription)
import Apropos.Logic (Formula (..), enumerateScenariosWhere, satisfiesFormula)
import Data.String (fromString)
import Hedgehog (Group (..), Property, PropertyT, property, (===), forAll)
import Hedgehog.Internal.Property (unPropertyT, PropertyT (PropertyT), TestT (unTest, TestT))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Either (isRight)

runPureTest :: forall d a. (Description d a, DeepHasDatatypeInfo d, Show a) => Formula (VariableRep d) -> (a -> PropertyT IO ()) -> d -> Property
runPureTest expect script d = property $ do
  a <- forAll $ genForDescription d
  b <- passes (script a)
  b === sat
    where
      sat :: Bool
      sat = satisfiesFormula expect d

      passes :: PropertyT IO () -> PropertyT IO Bool
      passes =
        PropertyT
          . TestT 
          . ExceptT 
          . fmap (Right . isRight) 
          . runExceptT 
          . unTest 
          . unPropertyT

runPureTestsWhere :: forall d a. (Show d, Show a, Description d a, DeepHasDatatypeInfo d) => Formula (VariableRep d) -> (a -> PropertyT IO ()) -> String -> Formula (VariableRep d) -> Group
runPureTestsWhere expect script name condition =
  Group (fromString name) $
    [ ( fromString $ show $ variablesToDescription scenario
      , runPureTest expect script (variablesToDescription scenario)
      )
    | scenario <- enumerateScenariosWhere condition
    ]
