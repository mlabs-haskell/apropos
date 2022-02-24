module Spec.TicTacToe.Player (
  PlayerProperty (..),
  playerPermutationGenSelfTest,
) where

import Apropos.Gen
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.HasPermutationGenerator
import Apropos.HasPermutationGenerator.Contract
import Apropos.LogicalModel
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (linear)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data PlayerProperty
  = PlayerIsX
  | PlayerIsO
  | PlayerIsInvalid
  deriving stock (Eq, Ord, Enum, Show, Bounded)

instance Enumerable PlayerProperty where
  enumerated = [minBound .. maxBound]

instance LogicalModel PlayerProperty where
  logic = (ExactlyOne $ Var <$> [PlayerIsInvalid, PlayerIsX, PlayerIsO])

instance HasLogicalModel PlayerProperty Int where
  satisfiesProperty PlayerIsX player = player == 1
  satisfiesProperty PlayerIsO player = player == 0
  satisfiesProperty PlayerIsInvalid player =
    not (satisfiesAny [PlayerIsX, PlayerIsO] player)

instance HasPermutationGenerator PlayerProperty Int where
  generators =
    [ PermutationEdge
        { name = "MakePlayerX"
        , match = Not $ Var PlayerIsX
        , contract = removeAll [PlayerIsO, PlayerIsInvalid] >> add PlayerIsX
        , permuteGen = do
            pure 1
        }
    , PermutationEdge
        { name = "MakePlayerO"
        , match = Not $ Var PlayerIsO
        , contract = removeAll [PlayerIsX, PlayerIsInvalid] >> add PlayerIsO
        , permuteGen = do
            pure 0
        }
    , PermutationEdge
        { name = "MakePlayerInvalid"
        , match = Not $ Var PlayerIsInvalid
        , contract = removeAll [PlayerIsX, PlayerIsO] >> add PlayerIsInvalid
        , permuteGen = do
            p <-
              liftGenPA $
                Gen.filter (\i -> not (i `elem` [0, 1])) $
                  Gen.int (linear minBound maxBound)
            pure p
        }
    ]

instance HasParameterisedGenerator PlayerProperty Int where
  parameterisedGenerator = buildGen baseGen

baseGen :: PGen Int
baseGen = liftGenP $ Gen.int (linear minBound maxBound)

playerPermutationGenSelfTest :: TestTree
playerPermutationGenSelfTest =
  testGroup "playerPermutationGenSelfTest" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: PermutationEdge PlayerProperty Int) -> True)
        baseGen
