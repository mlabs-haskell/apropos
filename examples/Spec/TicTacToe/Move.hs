module Spec.TicTacToe.Move (
  MoveProperty (..),
  movePermutationGenSelfTest,
) where

import Apropos
import Apropos.LogicalModel

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
  logic = abstractionLogic @(Prop MoveProperty)

instance HasLogicalModel MoveProperty (Int, Int) where
  satisfiesProperty (MoveLocation p) (_, location) = satisfiesProperty p location
  satisfiesProperty (MovePlayer p) (player, _) = satisfiesProperty p player

instance HasAbstractions (Prop MoveProperty) (Int, Int) where
  sourceAbstractions =
    [ SoAs $
        SourceAbstraction
          { sourceAbsName = "move source"
          , constructor = (,)
          , productAbs =
              ProductAbstraction
                { abstractionName = "MovePlayer"
                , propertyAbstraction = abstractsProperties (Prop . MovePlayer . unProp)
                , productModelAbstraction = _1
                }
                :& ProductAbstraction
                  { abstractionName = "MoveLocation"
                  , propertyAbstraction = abstractsProperties (Prop . MoveLocation . unProp)
                  , productModelAbstraction = _2
                  }
                :& Nil
          }
    ]

instance HasPermutationGenerator (Prop MoveProperty) (Int, Int) where
  sources = abstractionSources
  generators = abstractionMorphisms

instance HasParameterisedGenerator (Prop MoveProperty) (Int, Int) where
  parameterisedGenerator = buildGen @(Prop MoveProperty)

movePermutationGenSelfTest :: TestTree
movePermutationGenSelfTest =
  testGroup "movePermutationGenSelfTest" $
    pure $
      fromGroup $
        permutationGeneratorSelfTest @(Prop MoveProperty)
