{-# LANGUAGE TemplateHaskell #-}

module Spec.TicTacToe.MoveSequence (
  moveSequenceGenTests
  ) where
import Spec.TicTacToe.PlayerSequence
import Spec.TicTacToe.LocationSequence
import Spec.TicTacToe.PlayerLocationSequencePair
import Brutus.HasLogicalModel
import Brutus.LogicalModel
import Brutus.HasParameterisedGenerator
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import Control.Monad (join)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Proxy (Proxy(..))

data MoveSequenceProperty =
    MoveSequenceLocation LocationSequenceProperty
  | MoveSequencePlayer PlayerSequenceProperty
  deriving stock (Eq,Ord,Show)

$(gen_enumerable ''MoveSequenceProperty)

instance LogicalModel MoveSequenceProperty where
  logic = (MoveSequenceLocation <$> logic)
     :&&: (MoveSequencePlayer <$> logic)
     :&&: All [Var (MoveSequenceLocation LocationSequenceIsNull)
                    :<->: Var (MoveSequencePlayer PlayerSequenceNull)
              ,Var (MoveSequenceLocation LocationSequenceIsSingleton)
                    :<->: Var (MoveSequencePlayer PlayerSequenceSingleton)
              ,Var (MoveSequenceLocation LocationSequenceIsLongerThanGame)
                    :<->: Var (MoveSequencePlayer PlayerSequenceIsLongerThanGame)
              ]


instance HasLogicalModel MoveSequenceProperty [(Int,Int)] where
  satisfiesProperty (MoveSequenceLocation prop) mseq =
    satisfiesProperty prop (snd <$> mseq)
  satisfiesProperty (MoveSequencePlayer prop) mseq =
    satisfiesProperty prop (fst <$> mseq)

translateProp :: MoveSequenceProperty
              -> PlayerLocationSequencePairProperty
translateProp (MoveSequenceLocation p) = PlayerLocationSequencePairLocation p
translateProp (MoveSequencePlayer p)   = PlayerLocationSequencePairPlayer p

translateProps :: Set MoveSequenceProperty -> Set PlayerLocationSequencePairProperty
translateProps plspps =
  let b = Set.fromList (translateProp <$> Set.toList plspps)
   in Set.insert PlayerLocationSequencePairLengthsAreEqual b


instance HasParameterisedGenerator MoveSequenceProperty [(Int,Int)] where
  parameterisedGenerator = \ ps -> do
    (p,l) <- parameterisedGenerator (translateProps ps)
    pure $ zip p l

moveSequenceGenTests :: TestTree
moveSequenceGenTests = testGroup "Spec.MoveSequence" $
    fromGroup <$> [
      runGeneratorTestsWhere (Proxy :: Proxy [(Int,Int)]) "MoveSequence Generator" (Yes :: Formula MoveSequenceProperty)
    ]


