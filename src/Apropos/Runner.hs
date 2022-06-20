module Apropos.Runner (
  AproposTest(..),
  runTests,
  runTestsWhere,
) where

import Apropos.Description (DeepHasDatatypeInfo, Description (..), variablesToDescription, scenarios)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Either (isRight)
import Data.String (fromString)
import Hedgehog (Group (..), Property, PropertyT, forAll, property, (===))
import Hedgehog.Internal.Property (PropertyT (PropertyT), TestT (TestT, unTest), unPropertyT)

data AproposTest d a
  = AproposTest
  { expect :: d -> Bool 
  , test :: a -> PropertyT IO ()
  }

runTest :: forall d a. (Description d a, Show a) => AproposTest d a -> d -> Property
runTest atest d = property $ do
  a <- forAll $ genForDescription d
  b <- passes (test atest a)
  b === expect atest d
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

runTests :: forall d a. (Show d, Show a, Description d a, DeepHasDatatypeInfo d) => String -> AproposTest d a ->  Group
runTests name = runTestsWhere name (const True)

runTestsWhere :: forall d a. (Show d, Show a, Description d a, DeepHasDatatypeInfo d) =>  String -> (d -> Bool) -> AproposTest d a -> Group
runTestsWhere name cond atest =
  Group (fromString name) $
    [ ( fromString $ show scenario
      , runTest atest scenario
      )
    | scenario <- filter cond . map variablesToDescription $ scenarios
    ]
