{-# LANGUAGE TemplateHaskell #-}

module Spec.TicTacToe.PlayerLocationSequencePair (
  PlayerLocationSequencePairProperty (..),
  playerLocationSequencePairPermutationGenSelfTest,
) where

import Apropos.Gen
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.HasPermutationGenerator
import Apropos.LogicalModel
import Control.Monad (join)
import Spec.TicTacToe.LocationSequence
import Spec.TicTacToe.PlayerSequence
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import Control.Lens.Tuple(_1,_2)


data PlayerLocationSequencePairProperty
  = PlayerLocationSequencePairLocation LocationSequenceProperty
  | PlayerLocationSequencePairPlayer PlayerSequenceProperty
  | PlayerLocationSequencePairLengthsAreEqual
  deriving stock (Eq, Ord, Show)

$(gen_enumerable ''PlayerLocationSequencePairProperty)

instance LogicalModel PlayerLocationSequencePairProperty where
  logic =
    (PlayerLocationSequencePairLocation <$> logic)
      :&&: (PlayerLocationSequencePairPlayer <$> logic)
      :&&: Var PlayerLocationSequencePairLengthsAreEqual
      :&&: ( Var PlayerLocationSequencePairLengthsAreEqual
              :<->: All
                [ Var (PlayerLocationSequencePairLocation LocationSequenceIsNull)
                    :<->: Var (PlayerLocationSequencePairPlayer PlayerSequenceNull)
                , Var (PlayerLocationSequencePairLocation LocationSequenceIsSingleton)
                    :<->: Var (PlayerLocationSequencePairPlayer PlayerSequenceSingleton)
                , Var (PlayerLocationSequencePairLocation LocationSequenceIsLongerThanGame)
                    :<->: Var (PlayerLocationSequencePairPlayer PlayerSequenceIsLongerThanGame)
                ]
           )

instance HasLogicalModel PlayerLocationSequencePairProperty ([Int], [Int]) where
  satisfiesProperty (PlayerLocationSequencePairLocation prop) mseq =
    satisfiesProperty prop (snd mseq)
  satisfiesProperty (PlayerLocationSequencePairPlayer prop) mseq =
    satisfiesProperty prop (fst mseq)
  satisfiesProperty PlayerLocationSequencePairLengthsAreEqual (p, l) = length p == length l

instance HasPermutationGenerator PlayerLocationSequencePairProperty ([Int], [Int]) where
  generators =
    let l = Abstraction { abstractionName = ""
                        , propertyAbstraction = abstractsProperties PlayerLocationSequencePairPlayer
                        , modelAbstraction = _1
                        }
        r = Abstraction { abstractionName = ""
                        , propertyAbstraction = abstractsProperties PlayerLocationSequencePairLocation
                        , modelAbstraction = _2
                        }
     in (abstract l <$> generators) |:-> (abstract r <$> generators)

instance HasParameterisedGenerator PlayerLocationSequencePairProperty ([Int], [Int]) where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen ([Int], [Int])
baseGen = do
  l <- int (linear 0 10)
  l1 <- list (singleton l) $ int (linear minBound maxBound)
  l2 <- list (singleton l) $ int (linear minBound maxBound)
  pure (l1, l2)

playerLocationSequencePairPermutationGenSelfTest :: TestTree
playerLocationSequencePairPermutationGenSelfTest =
  testGroup "playerLocationSequencePairPermutationGenSelfTest" $
    fromGroup
      <$> permutationGeneratorSelfTest
        False
        (\(_ :: Morphism PlayerLocationSequencePairProperty ([Int], [Int])) -> True)
        baseGen
