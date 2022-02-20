module Spec.TicTacToe.Player (
  ticTacToePlayerGenSelfTests
  ) where
import Proper.HasLogicalModel
import Proper.LogicalModel
import Proper.HasParameterisedGenerator
import Proper.HasPermutationGenerator
import Proper.HasPermutationGenerator.Contract
import Proper.HasPermutationGenerator.Gen
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (linear)
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data PlayerProperty =
      IsPlayerX
    | IsPlayerO
    | NotAPlayer
    deriving stock (Eq,Ord,Enum,Show,Bounded)

instance Enumerable PlayerProperty where
  enumerated = [minBound..maxBound]

instance LogicalModel PlayerProperty where
  logic = ExactlyOne $ Var <$> [IsPlayerX,IsPlayerO,NotAPlayer]

instance HasLogicalModel PlayerProperty Integer where
  satisfiesProperty IsPlayerX i = i == 1
  satisfiesProperty IsPlayerO i = i == 2
  satisfiesProperty NotAPlayer i = not (i `elem` [1,2])

instance HasPermutationGenerator PlayerProperty Integer where
  generators =
    [ PermutationEdge
      { name = "MakePlayerX"
      , match = Not $ Var IsPlayerX
      , contract = clear >> add IsPlayerX
      , permuteGen = pure 1
      }
    , PermutationEdge
      { name = "MakePlayerO"
      , match = Not $ Var IsPlayerO
      , contract = clear >> add IsPlayerO
      , permuteGen = pure 2
      }
    , PermutationEdge
      { name = "MakeNotAPlayer"
      , match = Not $ Var NotAPlayer
      , contract = clear >> add NotAPlayer
      , permuteGen = liftGenPA $ fromIntegral <$> (Gen.choice [Gen.int (linear minBound 0)
                                                           ,Gen.int (linear 3 maxBound)])
      }

    ]

instance HasParameterisedGenerator PlayerProperty Integer where
  parameterisedGenerator = buildGen baseGen

baseGen :: PGen Integer
baseGen = liftGenP (fromIntegral <$> Gen.int (linear minBound maxBound))

ticTacToePlayerGenSelfTests :: TestTree
ticTacToePlayerGenSelfTests = testGroup "TicTacToe Player permutationGeneratorSelfTest" $
  fromGroup <$> permutationGeneratorSelfTest (\(_ :: PermutationEdge PlayerProperty Integer) -> True) baseGen

