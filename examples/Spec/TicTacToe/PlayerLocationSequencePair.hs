module Spec.TicTacToe.PlayerLocationSequencePair (
  PlayerLocationSequencePairProperty (..),
  playerLocationSequencePairPermutationGenSelfTest,
) where

import Apropos as A
import Apropos.LogicalModel as LM
import Control.Lens.Tuple (_1, _2)
import Spec.TicTacToe.LocationSequence
import Spec.TicTacToe.PlayerSequence
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data PlayerLocationSequencePairProperty
  = PlayerLocationSequencePairLocation (Prop LocationSequenceProperty)
  | PlayerLocationSequencePairPlayer (Prop PlayerSequenceProperty)
  | PlayerLocationSequencePairLengthsAreEqual
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel PlayerLocationSequencePairProperty where
  logic =
    (PlayerLocationSequencePairLocation <$> A.logic)
      :&&: (PlayerLocationSequencePairPlayer <$> A.logic)
      :&&: Var PlayerLocationSequencePairLengthsAreEqual
      :&&: ( Var PlayerLocationSequencePairLengthsAreEqual
              :<->: All
                [ Var (PlayerLocationSequencePairLocation (Prop LocationSequenceIsNull))
                    :<->: Var (PlayerLocationSequencePairPlayer (Prop PlayerSequenceNull))
                , Var (PlayerLocationSequencePairLocation (Prop LocationSequenceIsSingleton))
                    :<->: Var (PlayerLocationSequencePairPlayer (Prop PlayerSequenceSingleton))
                , Var (PlayerLocationSequencePairLocation (Prop LocationSequenceIsLongerThanGame))
                    :<->: Var (PlayerLocationSequencePairPlayer (Prop PlayerSequenceIsLongerThanGame))
                ]
           )

instance HasLogicalModel PlayerLocationSequencePairProperty ([Int], [Int]) where
  satisfiesProperty (PlayerLocationSequencePairLocation (Prop prop)) mseq =
    satisfiesProperty prop (snd mseq)
  satisfiesProperty (PlayerLocationSequencePairPlayer (Prop prop)) mseq =
    satisfiesProperty prop (fst mseq)
  satisfiesProperty PlayerLocationSequencePairLengthsAreEqual (p, l) = length p == length l

instance HasAbstractions (Prop PlayerLocationSequencePairProperty) ([Int], [Int]) where
  sourceAbstractions =
    [ SoAs $
        SourceAbstraction
          { sourceAbsName = ""
          , constructor = (,)
          , productAbs =
              ProductAbstraction
                { abstractionName = ""
                , propertyAbstraction = abstractsProperties (Prop . PlayerLocationSequencePairPlayer)
                , productModelAbstraction = _1
                }
                :& ProductAbstraction
                  { abstractionName = ""
                  , propertyAbstraction = abstractsProperties (Prop . PlayerLocationSequencePairLocation)
                  , productModelAbstraction = _2
                  }
                :& Nil
          }
    ]

instance HasPermutationGenerator (Prop PlayerLocationSequencePairProperty) ([Int], [Int]) where
  sources = abstractionSources
  generators = abstractionMorphisms ++ parallelAbstractionMorphisms

instance HasParameterisedGenerator (Prop PlayerLocationSequencePairProperty) ([Int], [Int]) where
  parameterisedGenerator = buildGen

playerLocationSequencePairPermutationGenSelfTest :: TestTree
playerLocationSequencePairPermutationGenSelfTest =
  testGroup "playerLocationSequencePairPermutationGenSelfTest" $
    pure $
      fromGroup $
        permutationGeneratorSelfTest @(Prop PlayerLocationSequencePairProperty)
