module Spec.TicTacToe.PlayerLocationSequencePair (
  PlayerLocationSequencePairProperty (..),
  playerLocationSequencePairPermutationGenSelfTest,
) where

import Apropos
import Control.Lens.Tuple (_1, _2)
import Spec.TicTacToe.LocationSequence
import Spec.TicTacToe.PlayerSequence
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data PlayerLocationSequencePairProperty
  = PlayerLocationSequencePairLocation LocationSequenceProperty
  | PlayerLocationSequencePairPlayer PlayerSequenceProperty
  | PlayerLocationSequencePairLengthsAreEqual
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

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

instance HasAbstractions PlayerLocationSequencePairProperty ([Int], [Int]) where
  sourceAbstractions =
    [ SoAs $
        SourceAbstraction
          { sourceAbsName = ""
          , constructor = (,)
          , productAbs =
              ProductAbstraction
                { abstractionName = ""
                , propertyAbstraction = abstractsProperties PlayerLocationSequencePairPlayer
                , productModelAbstraction = _1
                }
                :& ProductAbstraction
                  { abstractionName = ""
                  , propertyAbstraction = abstractsProperties PlayerLocationSequencePairLocation
                  , productModelAbstraction = _2
                  }
                :& Nil
          }
    ]

instance HasPermutationGenerator PlayerLocationSequencePairProperty ([Int], [Int]) where
  sources = abstractionSources
  generators = abstractionMorphisms ++ parallelAbstractionMorphisms

instance HasParameterisedGenerator PlayerLocationSequencePairProperty ([Int], [Int]) where
  parameterisedGenerator = buildGen

playerLocationSequencePairPermutationGenSelfTest :: TestTree
playerLocationSequencePairPermutationGenSelfTest =
  testGroup "playerLocationSequencePairPermutationGenSelfTest" $
    pure $
      fromGroup $
        permutationGeneratorSelfTest (Apropos :: ([Int], [Int]) :+ PlayerLocationSequencePairProperty)
