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
  logic = ExactlyOne $ Var <$> [PlayerIsInvalid, PlayerIsX, PlayerIsO]

instance HasLogicalModel PlayerProperty Int where
  satisfiesProperty PlayerIsX player = player == 1
  satisfiesProperty PlayerIsO player = player == 0
  satisfiesProperty PlayerIsInvalid player =
    not (satisfiesAny [PlayerIsX, PlayerIsO] player)

instance HasPermutationGenerator PlayerProperty Int where
  generators =
    [ Morphism
        { name = "MakePlayerX"
        , match = Not $ Var PlayerIsX
        , contract = removeAll [PlayerIsO, PlayerIsInvalid] >> add PlayerIsX
        , morphism = \_ -> pure 1
        }
    , Morphism
        { name = "MakePlayerO"
        , match = Not $ Var PlayerIsO
        , contract = removeAll [PlayerIsX, PlayerIsInvalid] >> add PlayerIsO
        , morphism = \_ -> pure 0
        }
    , Morphism
        { name = "MakePlayerInvalid"
        , match = Not $ Var PlayerIsInvalid
        , contract = removeAll [PlayerIsX, PlayerIsO] >> add PlayerIsInvalid
        , morphism = \_ ->
            genFilter (\i -> i `notElem` [0, 1]) $
              int (linear minBound maxBound)
        }
    ]

instance HasParameterisedGenerator PlayerProperty Int where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen Int
baseGen = int (linear minBound maxBound)

playerPermutationGenSelfTest :: TestTree
playerPermutationGenSelfTest =
  testGroup "playerPermutationGenSelfTest" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: Morphism PlayerProperty Int) -> True)
        baseGen
