{-# LANGUAGE TemplateHaskell #-}

module Spec.TicTacToe.PlayerLocationSequencePair (
  PlayerLocationSequencePairProperty(..),
  playerLocationSequencePairPermutationGenSelfTest,
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

data PlayerLocationSequencePairProperty =
    PlayerLocationSequencePairLocation LocationSequenceProperty
  | PlayerLocationSequencePairPlayer PlayerSequenceProperty
  | PlayerLocationSequencePairLengthsAreEqual
  deriving stock (Eq,Ord,Show)

$(gen_enumerable ''PlayerLocationSequencePairProperty)

instance LogicalModel PlayerLocationSequencePairProperty where
  logic = (PlayerLocationSequencePairLocation <$> logic)
     :&&: (PlayerLocationSequencePairPlayer <$> logic)
     :&&: Var PlayerLocationSequencePairLengthsAreEqual
     :&&: (Var PlayerLocationSequencePairLengthsAreEqual :<->:
             All [Var (PlayerLocationSequencePairLocation LocationSequenceIsNull)
                    :<->: Var (PlayerLocationSequencePairPlayer PlayerSequenceNull)
                 ,Var (PlayerLocationSequencePairLocation LocationSequenceIsSingleton)
                    :<->: Var (PlayerLocationSequencePairPlayer PlayerSequenceSingleton)
                 ,Var (PlayerLocationSequencePairLocation LocationSequenceIsLongerThanGame)
                    :<->: Var (PlayerLocationSequencePairPlayer PlayerSequenceIsLongerThanGame)
                 ])

instance HasLogicalModel PlayerLocationSequencePairProperty ([Int],[Int]) where
  satisfiesProperty (PlayerLocationSequencePairLocation prop) mseq =
    satisfiesProperty prop (snd mseq)
  satisfiesProperty (PlayerLocationSequencePairPlayer prop) mseq =
    satisfiesProperty prop (fst mseq)
  satisfiesProperty PlayerLocationSequencePairLengthsAreEqual (p,l) = length p == length l

instance HasPermutationGenerator PlayerLocationSequencePairProperty ([Int],[Int]) where
  generators =
    let l = liftEdges PlayerLocationSequencePairPlayer
                      fst
                      (\f moves-> (f, snd moves))
                      (\p -> case p of
                               (PlayerLocationSequencePairPlayer q) -> Just q
                               _ -> Nothing)
                      "PlayerLocationSequencePairPlayer"
                      generators
        r = liftEdges PlayerLocationSequencePairLocation
                      snd
                      (\f moves -> (fst moves, f))
                      (\p -> case p of
                               (PlayerLocationSequencePairLocation q) -> Just q
                               _ -> Nothing)
                      "PlayerLocationSequencePairLocation"
                      generators
     in [ composeEdges l' r' | l' <- l, r' <- r ]

instance HasParameterisedGenerator PlayerLocationSequencePairProperty ([Int],[Int]) where
  parameterisedGenerator = buildGen baseGen

baseGen :: PGen ([Int],[Int])
baseGen = do
  l <- liftGenP $ Gen.int (linear 0 10)
  l1 <- liftGenP $ Gen.list (singleton l) $ Gen.int (linear minBound maxBound)
  l2 <- liftGenP $ Gen.list (singleton l) $ Gen.int (linear minBound maxBound)
  pure (l1,l2)

playerLocationSequencePairPermutationGenSelfTest :: TestTree
playerLocationSequencePairPermutationGenSelfTest =
  testGroup "playerLocationSequencePairPermutationGenSelfTest" $
    fromGroup <$> permutationGeneratorSelfTest False
               (\(_ :: PermutationEdge PlayerLocationSequencePairProperty ([Int],[Int])) -> True)
               baseGen

