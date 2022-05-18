module Spec.TicTacToe.Move (
  MoveProperty (..),
  movePermutationGenSelfTest,
) where

import Apropos
import Control.Lens.Tuple (_1, _2)
import Spec.TicTacToe.Location
import Spec.TicTacToe.Player
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data MoveProperty
  = MoveLocation LocationProperty
  | MovePlayer PlayerProperty
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel MoveProperty where
  logic = abstractionLogic @(Int, Int)

instance HasLogicalModel MoveProperty (Int, Int) where
  satisfiesProperty (MoveLocation prop) (_, location) = satisfiesProperty prop location
  satisfiesProperty (MovePlayer prop) (player, _) = satisfiesProperty prop player

instance HasAbstractions MoveProperty (Int, Int) where
  sourceAbstractions =
    [ SoAs $
        SourceAbstraction
          { sourceAbsName = "move source"
          , constructor = (,)
          , productAbs =
              ProductAbstraction
                { abstractionName = "MovePlayer"
                , propertyAbstraction = abstractsProperties MovePlayer
                , productModelAbstraction = _1
                }
                :& ProductAbstraction
                  { abstractionName = "MoveLocation"
                  , propertyAbstraction = abstractsProperties MoveLocation
                  , productModelAbstraction = _2
                  }
                :& Nil
          }
    ]

instance HasPermutationGenerator MoveProperty (Int, Int) where
  sources = abstractionSources
  generators = abstractionMorphisms

instance HasParameterisedGenerator MoveProperty (Int, Int) where
  parameterisedGenerator = buildGen

movePermutationGenSelfTest :: TestTree
movePermutationGenSelfTest =
  testGroup "movePermutationGenSelfTest" $
    pure $
      fromGroup $
        permutationGeneratorSelfTest @MoveProperty
