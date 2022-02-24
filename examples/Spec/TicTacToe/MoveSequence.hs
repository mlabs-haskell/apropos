{-# LANGUAGE TemplateHaskell #-}

module Spec.TicTacToe.MoveSequence (
  moveSequencePermutationGenSelfTest
  ) where
import Spec.TicTacToe.LocationSequence
import Apropos.HasLogicalModel
import Apropos.LogicalModel
import Apropos.HasPermutationGenerator
import Apropos.HasPermutationGenerator.Contract
import Apropos.HasParameterisedGenerator
import Apropos.Gen
import qualified Hedgehog.Gen as Gen
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import Control.Monad (join)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans (lift)
import Data.List (transpose)

data MoveSequenceProperty =
    MoveSequenceValid
  | MoveSequenceContainsWin
  deriving stock (Eq,Ord,Show)

$(gen_enumerable ''MoveSequenceProperty)

splitPlayers :: [Int] -> ([Int],[Int])
splitPlayers locationSeq = go locationSeq ([],[])
  where
    go [] p = p
    go [s] (a,b) = (s:a,b)
    go (h:i:j) (a,b) = go j (h:a,i:b)

containsWin :: [Int] -> Bool
containsWin locationSeq =
  let (x,o) = splitPlayers locationSeq
    in any (all (`elem` x)) winTileSets || any (all (`elem` o)) winTileSets

winTileSets :: [Set Int]
winTileSets = Set.fromList <$> [[0,1,2],[3,4,5],[6,7,8]
                               ,[0,3,6],[1,4,7],[2,5,8]
                               ,[0,4,8],[2,4,6]
                               ]

instance LogicalModel MoveSequenceProperty where
  logic = Yes

locationSequenceIsValid :: Formula LocationSequenceProperty
locationSequenceIsValid = Var AllLocationsAreInBounds
                     :&&: (Not $ Var SomeLocationIsOccupiedTwice)

instance HasLogicalModel MoveSequenceProperty [Int] where
  satisfiesProperty MoveSequenceValid ms = satisfiesExpression locationSequenceIsValid ms
  satisfiesProperty MoveSequenceContainsWin ms = containsWin ms

baseGen :: PGen [Int]
baseGen = genSatisfying (Yes :: Formula LocationSequenceProperty)

instance HasPermutationGenerator MoveSequenceProperty [Int] where
  generators =
    [ PermutationEdge
      { name = "InvalidNoWin"
      , match = Yes
      , contract = clear
      , permuteGen = filterPA (not . containsWin) $ lift $ genSatisfying $ Not locationSequenceIsValid
      }
    , PermutationEdge
      { name = "ValidNoWin"
      , match = Yes
      , contract = clear >> add MoveSequenceValid
      , permuteGen = filterPA (not . containsWin) $ lift $ genSatisfying locationSequenceIsValid
      }
    , PermutationEdge
      { name = "InvalidWin"
      , match = Not $ Var MoveSequenceValid
      , contract = add MoveSequenceContainsWin
      , permuteGen = do
          moves <- ask
          winlocs <- Set.toList <$> (liftGenPA $ Gen.element winTileSets)
          whofirst <- liftGenPA $ Gen.element [[moves,winlocs],[winlocs,moves]]
          let win = join $ transpose whofirst
          if containsWin win && satisfiesExpression (Not locationSequenceIsValid) win
             then pure win
             -- in general rejection sampling is to be avoided but if you feel like you need it
             -- this will "retry" the permutationGenerator from the top
             -- there is no path length limit at the moment so something like this might never terminate
             else lift $ genSatisfying ((Not $ Var MoveSequenceValid) :&&: Var MoveSequenceContainsWin)
      }
    , PermutationEdge
      { name = "ValidWin"
      , match = Var MoveSequenceValid
      , contract = add MoveSequenceContainsWin
      , permuteGen = do
          moves <- ask
          winlocs <- Set.toList <$> (liftGenPA $ Gen.element winTileSets)
          whofirst <- liftGenPA $ Gen.element [[moves,winlocs],[winlocs,moves]]
          let win = join $ transpose whofirst
          if containsWin win && satisfiesExpression locationSequenceIsValid win
             then pure win
             -- in general rejection sampling is to be avoided but if you feel like you need it
             -- this will "retry" the permutationGenerator from the top
             -- there is no path length limit at the moment so something like this might never terminate
             else lift $ genSatisfying (Var MoveSequenceValid :&&: Var MoveSequenceContainsWin)
      }
    ]

instance HasParameterisedGenerator MoveSequenceProperty [Int] where
  parameterisedGenerator = buildGen baseGen

moveSequencePermutationGenSelfTest :: TestTree
moveSequencePermutationGenSelfTest = testGroup "moveSequencePermutationGenSelfTest" $
  fromGroup <$> permutationGeneratorSelfTest
                 True
                 (\(_ :: PermutationEdge MoveSequenceProperty [Int]) -> True)
                 baseGen

