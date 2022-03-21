module Spec.TicTacToe.Move (
  MoveProperty (..),
  movePermutationGenSelfTest,
) where

import Apropos.Gen
import Apropos.HasAbstractions
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.HasPermutationGenerator
import Apropos.LogicalModel
import Control.Lens.Tuple (_1, _2)
import GHC.Generics (Generic)
import Spec.TicTacToe.Location
import Spec.TicTacToe.Player
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data MoveProperty
  = MoveLocation LocationProperty
  | MovePlayer PlayerProperty
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable)

instance LogicalModel MoveProperty where
  logic = abstractionLogic @(Int, Int)

instance HasLogicalModel MoveProperty (Int, Int) where
  satisfiesProperty (MoveLocation prop) (_, location) = satisfiesProperty prop location
  satisfiesProperty (MovePlayer prop) (player, _) = satisfiesProperty prop player

instance HasAbstractions MoveProperty (Int, Int) where
  abstractions =
    [ WrapAbs $
        ProductAbstraction
          { abstractionName = "MovePlayer"
          , propertyAbstraction = abstractsProperties MovePlayer
          , productModelAbstraction = _1
          }
    , WrapAbs $
        ProductAbstraction
          { abstractionName = "MoveLocation"
          , propertyAbstraction = abstractsProperties MoveLocation
          , productModelAbstraction = _2
          }
    ]

instance HasPermutationGenerator MoveProperty (Int, Int) where
  generators = abstractionGenerators

instance HasParameterisedGenerator MoveProperty (Int, Int) where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen (Int, Int)
baseGen =
  let g = int (linear minBound maxBound)
   in (,) <$> g <*> g

movePermutationGenSelfTest :: TestTree
movePermutationGenSelfTest =
  testGroup "movePermutationGenSelfTest" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: Morphism MoveProperty (Int, Int)) -> True)
        baseGen
