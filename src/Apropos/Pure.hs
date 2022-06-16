module Apropos.Pure (
  runPureTest,
  runPureTestsWhere,
) where

import Apropos.Description (DeepHasDatatypeInfo, Description (..), VariableRep, variablesToDescription)
import Apropos.Gen (liftGenModifiable, runTest)
import Apropos.Logic (Formula (..), enumerateScenariosWhere, satisfiesFormula)
import Data.String (fromString)
import Hedgehog (Group (..), Property, PropertyT, (===))
import Control.Monad.Trans (lift)
import Hedgehog.Internal.Property (unPropertyT, runTestT)
import Hedgehog.Internal.Gen (evalGenT)
import qualified Hedgehog.Internal.Seed as Seed
import Hedgehog.Internal.Tree
import Control.Monad.IO.Class (liftIO)

runPureTest :: forall d a. (Description d a, DeepHasDatatypeInfo d) => Formula (VariableRep d) -> (a -> PropertyT IO ()) -> d -> Property
runPureTest expect script d =
  runTest
    (descriptionGen d)
    (liftGenModifiable 
      . lift 
      . lift . \a -> do
          b <- liftIO $ passes (script a)
          b === sat 
    )
  where
    sat :: Bool
    sat = satisfiesFormula expect d

    passes :: forall b. PropertyT IO b -> IO Bool
    passes prop = do
      seed <- Seed.random
      m <- runTreeT . evalGenT 0 seed . runTestT . unPropertyT $ prop
      case nodeValue m of
        Just (Right _, _) -> pure True
        _ -> pure False

runPureTestsWhere :: forall d a. (Show d, Description d a, DeepHasDatatypeInfo d) => Formula (VariableRep d) -> (a -> PropertyT IO ()) -> String -> Formula (VariableRep d) -> Group
runPureTestsWhere expect script name condition =
  Group (fromString name) $
    [ ( fromString $ show $ variablesToDescription scenario
      , runPureTest expect script (variablesToDescription scenario)
      )
    | scenario <- enumerateScenariosWhere condition
    ]