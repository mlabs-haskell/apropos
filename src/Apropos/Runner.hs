module Apropos.Runner (
  AproposTest (..),
  runTests,
  runTestsWhere,
) where

import Apropos.Description (DeepHasDatatypeInfo, Description (..), scenarios, variablesToDescription)
import Apropos.Generator (runTest)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Either (isRight)
import Data.Set qualified as Set
import Data.String (IsString, fromString)
import Hedgehog (Property, PropertyT, (===))
import Hedgehog.Internal.Property (PropertyT (PropertyT), TestT (TestT, unTest), unPropertyT)

data AproposTest d a = AproposTest
  { expect :: d -> Bool
  , test :: a -> PropertyT IO ()
  }

runAproposTest :: forall d a. (Description d a, Show a) => AproposTest d a -> d -> Property
runAproposTest atest d =
  runTest
    ( \a -> do
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

runTests :: forall d a s. (Show d, Show a, Ord d, Description d a, DeepHasDatatypeInfo d, IsString s) => AproposTest d a -> [(s, Property)]
runTests = runTestsWhere (const True)

runTestsWhere :: forall d a s. (Show d, Show a, Ord d, Description d a, DeepHasDatatypeInfo d, IsString s) => (d -> Bool) -> AproposTest d a -> [(s, Property)]
runTestsWhere cond atest =
  [ ( fromString $ show scenario
    , runAproposTest atest scenario
    )
  | scenario <- Set.toList . Set.filter cond . Set.map variablesToDescription $ scenarios
  ]
