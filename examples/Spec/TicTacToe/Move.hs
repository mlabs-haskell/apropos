{-# LANGUAGE TemplateHaskell #-}

module Spec.TicTacToe.Move (
  MoveProperty (..),
  movePermutationGenSelfTest,
) where

import Apropos.Gen
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.HasPermutationGenerator
import Apropos.LogicalModel
import Control.Monad (join)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (linear)
import Spec.TicTacToe.Location
import Spec.TicTacToe.Player
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data MoveProperty
  = MoveLocation LocationProperty
  | MovePlayer PlayerProperty
  deriving stock (Eq, Ord, Show)

$(gen_enumerable ''MoveProperty)

instance LogicalModel MoveProperty where
  logic = (MoveLocation <$> logic) :&&: (MovePlayer <$> logic)

instance HasLogicalModel MoveProperty (Int, Int) where
  satisfiesProperty (MoveLocation prop) (_, location) = satisfiesProperty prop location
  satisfiesProperty (MovePlayer prop) (player, _) = satisfiesProperty prop player

instance HasPermutationGenerator MoveProperty (Int, Int) where
  generators =
    let l = ModelAbstraction { abstractionName = "MovePlayer"
                             , projectProperty =  \p -> case p of
                                                          (MovePlayer q) -> Just q
                                                          _ -> Nothing
                             , injectProperty = MovePlayer
                             , projectSubmodel = fst
                             , injectSubmodel = \f (_, r') -> (f, r')
                             }
        r = ModelAbstraction { abstractionName = "MoveLocation"
                             , projectProperty =  \p -> case p of
                                                          (MoveLocation q) -> Just q
                                                          _ -> Nothing
                             , injectProperty = MoveLocation
                             , projectSubmodel = snd
                             , injectSubmodel = \f (l', _) -> (l', f)
                             }
     in join [l <$$> generators, r <$$> generators]

instance HasParameterisedGenerator MoveProperty (Int, Int) where
  parameterisedGenerator = buildGen baseGen

baseGen :: PGen (Int, Int)
baseGen =
  let g = Gen.int (linear minBound maxBound)
   in liftGenP ((,) <$> g <*> g)

movePermutationGenSelfTest :: TestTree
movePermutationGenSelfTest =
  testGroup "movePermutationGenSelfTest" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: PermutationEdge MoveProperty (Int, Int)) -> True)
        baseGen
