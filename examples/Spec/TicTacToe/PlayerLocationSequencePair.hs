module Spec.TicTacToe.PlayerLocationSequencePair (
  PlayerLocationSequencePairProperty (..),
  -- playerLocationSequencePairPermutationGenSelfTest,
) where

import Apropos
import Apropos.LogicalModel as LM
import Spec.TicTacToe.LocationSequence
import Spec.TicTacToe.PlayerSequence

data PlayerLocationSequencePairProperty
  = PlayerLocationSequencePairLocation LocationSequenceProperty
  | PlayerLocationSequencePairPlayer PlayerSequenceProperty
  | PlayerLocationSequencePairLengthsAreEqual
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel PlayerLocationSequencePairProperty where
  logic =
    (PlayerLocationSequencePairLocation <$> LM.logic)
      :&&: (PlayerLocationSequencePairPlayer <$> LM.logic)
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
  satisfiesProperty (PlayerLocationSequencePairLocation p) mseq =
    satisfiesProperty p (snd mseq)
  satisfiesProperty (PlayerLocationSequencePairPlayer p) mseq =
    satisfiesProperty p (fst mseq)
  satisfiesProperty PlayerLocationSequencePairLengthsAreEqual (p, l) = length p == length l

-- instance HasAbstractions (Prop PlayerLocationSequencePairProperty) ([Int], [Int]) where
--   sourceAbstractions =
--     [ SoAs $
--         SourceAbstraction
--           { sourceAbsName = ""
--           , constructor = (,)
--           , productAbs =
--               ProductAbstraction
--                 { abstractionName = ""
--                 , propertyAbstraction = abstractsProperties (Prop . PlayerLocationSequencePairPlayer . unProp)
--                 , productModelAbstraction = _1
--                 }
--                 :& ProductAbstraction
--                   { abstractionName = ""
--                   , propertyAbstraction = abstractsProperties (Prop . PlayerLocationSequencePairLocation . unProp)
--                   , productModelAbstraction = _2
--                   }
--                 :& Nil
--           }
--     ]

-- instance HasPermutationGenerator (Prop PlayerLocationSequencePairProperty) ([Int], [Int]) where
--   sources = abstractionSources
--   generators = abstractionMorphisms ++ parallelAbstractionMorphisms

instance HasParameterisedGenerator (Prop PlayerLocationSequencePairProperty) ([Int], [Int]) where
  parameterisedGenerator = undefined
  -- parameterisedGenerator = buildGen @(Prop PlayerLocationSequencePairProperty)

-- playerLocationSequencePairPermutationGenSelfTest :: TestTree
-- playerLocationSequencePairPermutationGenSelfTest =
--   testGroup "playerLocationSequencePairPermutationGenSelfTest" $
--     pure $
--       fromGroup $
--         permutationGeneratorSelfTest @(Prop PlayerLocationSequencePairProperty)
