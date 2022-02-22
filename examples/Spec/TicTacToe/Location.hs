module Spec.TicTacToe.Location (
  LocationProperty(..),
  locationPermutationGenSelfTest,
  ) where
import Brutus.HasLogicalModel
import Brutus.LogicalModel
import Brutus.HasParameterisedGenerator
import Brutus.HasPermutationGenerator
import Brutus.HasPermutationGenerator.Contract
import Brutus.Gen
--import Brutus.Pure.HasRunner
--import Brutus.Plutus.HasScriptRunner
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (linear)
--import Data.Proxy (Proxy(..))
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)
--import Plutarch (compile)
--import Plutarch.Prelude



data LocationProperty =
    LocationIsWithinBounds
  | LocationIsOutOfBounds
  deriving stock (Eq,Ord,Enum,Show,Bounded)

instance Enumerable LocationProperty where
  enumerated = [minBound..maxBound]

instance LogicalModel LocationProperty where
  logic = (ExactlyOne $ Var <$> [LocationIsWithinBounds,LocationIsOutOfBounds])

instance HasLogicalModel LocationProperty Int where
  satisfiesProperty LocationIsWithinBounds location = location >= 0 && location < 9
  satisfiesProperty LocationIsOutOfBounds  location =
    not (satisfiesProperty LocationIsWithinBounds location)

instance HasPermutationGenerator LocationProperty Int where
  generators =
    [ PermutationEdge 
      { name = "MakeLocationIsWithinBounds"
      , match = Var LocationIsOutOfBounds
      , contract = remove LocationIsOutOfBounds >> add LocationIsWithinBounds
      , permuteGen = liftGenPA $ Gen.int (linear 0 8)
      }
    , PermutationEdge 
      { name = "MakeLocationIsOutOfBounds"
      , match = Var LocationIsWithinBounds
      , contract = remove LocationIsWithinBounds >> add LocationIsOutOfBounds
      , permuteGen = liftGenPA $ Gen.choice $ [Gen.int (linear minBound (-1))
                                              ,Gen.int (linear 9 maxBound)]
      }


    ]


instance HasParameterisedGenerator LocationProperty Int where
  parameterisedGenerator = buildGen baseGen

baseGen :: PGen Int
baseGen = liftGenP $ Gen.int (linear minBound maxBound)

locationPermutationGenSelfTest :: TestTree
locationPermutationGenSelfTest = testGroup "locationPermutationGenSelfTest" $
  fromGroup <$> permutationGeneratorSelfTest True
                   (\(_ :: PermutationEdge LocationProperty Int) -> True) baseGen


