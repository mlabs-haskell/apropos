{-# LANGUAGE TemplateHaskell #-}

module Spec.TicTacToe.MoveSequence (
  moveSequencePermutationGenSelfTest
  ) where
import Spec.TicTacToe.LocationSequence
import Spec.TicTacToe.PlayerSequence
import Brutus.HasLogicalModel
import Brutus.LogicalModel
import Brutus.HasParameterisedGenerator
import Brutus.HasPermutationGenerator
import Brutus.Gen
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (singleton,linear)
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import Control.Monad (join)

data MoveSequenceProperty =
    MoveSequenceLocation LocationSequenceProperty
  | MoveSequencePlayer PlayerSequenceProperty
  | MoveSequenceLengthsAreEqual
  deriving stock (Eq,Ord,Show)

$(gen_enumerable ''MoveSequenceProperty)

instance LogicalModel MoveSequenceProperty where
  logic = (MoveSequenceLocation <$> logic)
     :&&: (MoveSequencePlayer <$> logic)
     :&&: Var MoveSequenceLengthsAreEqual
     :&&: (Var MoveSequenceLengthsAreEqual :<->:
             All [Var (MoveSequenceLocation LocationSequenceIsNull)
                    :<->: Var (MoveSequencePlayer PlayerSequenceNull)
                 ,Var (MoveSequenceLocation LocationSequenceIsSingleton)
                    :<->: Var (MoveSequencePlayer PlayerSequenceSingleton)
                 ])

instance HasLogicalModel MoveSequenceProperty ([Int],[Int]) where
  satisfiesProperty (MoveSequenceLocation prop) mseq =
    satisfiesProperty prop (snd mseq)
  satisfiesProperty (MoveSequencePlayer prop) mseq =
    satisfiesProperty prop (fst mseq)
  satisfiesProperty MoveSequenceLengthsAreEqual (p,l) = length p == length l

instance HasPermutationGenerator MoveSequenceProperty ([Int],[Int]) where
  generators =
    let l = liftEdges MoveSequencePlayer
                      fst
                      (\f moves-> (f, snd moves))
                      (\p -> case p of
                               (MoveSequencePlayer q) -> Just q
                               _ -> Nothing)
                      "MoveSequencePlayer "
                      generators
        r = liftEdges MoveSequenceLocation
                      snd
                      (\f moves -> (fst moves, f))
                      (\p -> case p of
                               (MoveSequenceLocation q) -> Just q
                               _ -> Nothing)
                      "MoveSequenceLocation "
                      generators
     in [ composeEdges l' r' | l' <- l, r' <- r ]

instance HasParameterisedGenerator MoveSequenceProperty ([Int],[Int]) where
  parameterisedGenerator = buildGen baseGen

baseGen :: PGen ([Int],[Int])
baseGen = do
  l <- liftGenP $ Gen.int (linear 2 9)
  l1 <- liftGenP $ Gen.list (singleton l) $ Gen.int (linear minBound maxBound) 
  l2 <- liftGenP $ Gen.list (singleton l) $ Gen.int (linear minBound maxBound) 
  pure (l1,l2)

moveSequencePermutationGenSelfTest :: TestTree
moveSequencePermutationGenSelfTest = testGroup "movePermutationGenSelfTest" $
  fromGroup <$> permutationGeneratorSelfTest False
             (\(_ :: PermutationEdge MoveSequenceProperty ([Int],[Int])) -> True)
             baseGen


