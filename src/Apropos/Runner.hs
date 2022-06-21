module Apropos.Runner (
  AproposTest(..),
  runTests,
  runTestsWhere,
) where

import Apropos.Description (DeepHasDatatypeInfo, Description (..), variablesToDescription, scenarios)
import Apropos.Generator (runTest)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Either (isRight)
import Data.String (fromString)
import Hedgehog (Group (..), Property, PropertyT, (===))
import Hedgehog.Internal.Property (PropertyT (PropertyT), TestT (TestT, unTest), unPropertyT)
import qualified Data.Set as Set


data AproposTest d a
  = AproposTest
  { expect :: d -> Bool 
  , test :: a -> PropertyT IO ()
  }

runAproposTest :: forall d a. (Description d a, Show a) => AproposTest d a -> d -> Property
runAproposTest atest d = runTest (
  \a -> do
    b <- passes (test atest a)
    b === expect atest d
  )
  d
  where
    passes :: PropertyT IO () -> PropertyT IO Bool
    passes =
      PropertyT
        . TestT
        . ExceptT
        . fmap (Right . isRight)
        . runExceptT
        . unTest
        . unPropertyT



runTests :: forall d a. (Show d, Show a, Ord d, Description d a, DeepHasDatatypeInfo d) => String -> AproposTest d a ->  Group
runTests name = runTestsWhere name (const True)

runTestsWhere :: forall d a. (Show d, Show a, Ord d, Description d a, DeepHasDatatypeInfo d) =>  String -> (d -> Bool) -> AproposTest d a -> Group
runTestsWhere name cond atest =
  Group (fromString name) $
    [ ( fromString $ show scenario
      , runAproposTest atest scenario
      )
    | scenario <- Set.toList . Set.filter cond . Set.map variablesToDescription $ scenarios
    ]
