module Spec.TicTacToe.Move (
  MoveProperty (..),
  movePermutationGenSelfTest,
) where

import Apropos.Gen
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.HasPermutationGenerator
import Apropos.LogicalModel
import Control.Lens.Tuple (_1, _2)
import Control.Monad (join)
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
  logic = (MoveLocation <$> logic) :&&: (MovePlayer <$> logic)

instance HasLogicalModel MoveProperty (Int, Location) where
  satisfiesProperty (MoveLocation prop) (_, location) = satisfiesProperty prop location
  satisfiesProperty (MovePlayer prop) (player, _) = satisfiesProperty prop player

instance HasPermutationGenerator MoveProperty (Int, Location) where
  generators =
    let l =
          Abstraction
            { abstractionName = "MovePlayer"
            , propertyAbstraction = abstractsProperties MovePlayer
            , modelAbstraction = _1
            }
        r =
          Abstraction
            { abstractionName = "MoveLocation"
            , propertyAbstraction = abstractsProperties MoveLocation
            , modelAbstraction = _2
            }
     in join [abstract l <$> generators, abstract r <$> generators]

instance HasParameterisedGenerator MoveProperty (Int, Location) where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen (Int, Location)
baseGen =
  let gi = int (linear minBound maxBound)
      gl = do
        x <- int (linear minBound maxBound)
        y <- int (linear minBound maxBound)
        return $
          Location
            { row = x
            , column = y
            }
   in (,) <$> gi <*> gl

movePermutationGenSelfTest :: TestTree
movePermutationGenSelfTest =
  testGroup "movePermutationGenSelfTest" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: Morphism MoveProperty (Int, Location)) -> True)
        baseGen
