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
  = MoveLocation (Prop LocationProperty)
  | MovePlayer (Prop PlayerProperty)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel MoveProperty where
  logic = unProp <$> abstractionLogic @(Int, Int)

instance HasLogicalModel MoveProperty (Int, Int) where
  satisfiesProperty (MoveLocation (Prop prop)) (_, location) = satisfiesProperty prop location
  satisfiesProperty (MovePlayer (Prop prop)) (player, _) = satisfiesProperty prop player

instance HasAbstractions (Prop MoveProperty) (Int, Int) where
  sourceAbstractions =
    [ SoAs $
        SourceAbstraction
          { sourceAbsName = "move source"
          , constructor = (,)
          , productAbs =
              ProductAbstraction
                { abstractionName = "MovePlayer"
                , propertyAbstraction = abstractsProperties (Prop . MovePlayer)
                , productModelAbstraction = _1
                }
                :& ProductAbstraction
                  { abstractionName = "MoveLocation"
                  , propertyAbstraction = abstractsProperties (Prop . MoveLocation)
                  , productModelAbstraction = _2
                  }
                :& Nil
          }
    ]

instance HasPermutationGenerator (Prop MoveProperty) (Int, Int) where
  sources = abstractionSources
  generators = abstractionMorphisms

instance HasParameterisedGenerator (Prop MoveProperty) (Int, Int) where
  parameterisedGenerator = buildGen

movePermutationGenSelfTest :: TestTree
movePermutationGenSelfTest =
  testGroup "movePermutationGenSelfTest" $
    pure $
      fromGroup $
        permutationGeneratorSelfTest @(Prop MoveProperty)
