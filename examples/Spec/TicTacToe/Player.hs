module Spec.TicTacToe.Player (
  PlayerProperty (..),
  playerPermutationGenSelfTest,
) where

import Apropos
import Apropos.LogicalModel
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import Apropos.LogicalModel.HasLogicalModel (var)

data PlayerProperty
  = PlayerIsX
  | PlayerIsO
  | PlayerIsInvalid
  deriving stock (Eq, Ord, Enum, Show, Bounded, Generic)
  deriving anyclass (Hashable)

instance Enumerable PlayerProperty where
  enumerated = [minBound .. maxBound]

instance LogicalModel PlayerProperty where
  logic = ExactlyOne $ Var <$> [PlayerIsInvalid, PlayerIsX, PlayerIsO]

instance HasLogicalModel PlayerProperty Int where
  satisfiesProperty PlayerIsX player = player == 1
  satisfiesProperty PlayerIsO player = player == 0
  satisfiesProperty PlayerIsInvalid player =
    not (satisfiesAny [PlayerIsX, PlayerIsO] player)

instance HasPermutationGenerator (Prop PlayerProperty) Int where
  sources =
    [ Source
        { sourceName = "X"
        , covers = var PlayerIsX
        , gen = pure 1
        }
    , Source
        { sourceName = "O"
        , covers = var PlayerIsO
        , gen = pure 0
        }
    , Source
        { sourceName = "invalid"
        , covers = var PlayerIsInvalid
        , gen =
            genFilter (\i -> i `notElem` [0, 1]) $
              int (linear minBound maxBound)
        }
    ]

instance HasParameterisedGenerator (Prop PlayerProperty) Int where
  parameterisedGenerator = buildGen @(Prop PlayerProperty)

playerPermutationGenSelfTest :: TestTree
playerPermutationGenSelfTest =
  testGroup "playerPermutationGenSelfTest" $
    pure $
      fromGroup $
        permutationGeneratorSelfTest @(Prop PlayerProperty)
