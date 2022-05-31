module Spec.TicTacToe.MoveSequence (
  moveSequencePermutationGenSelfTest,
) where

import Apropos
import Apropos.LogicalModel
import Apropos.LogicalModel.HasLogicalModel (var)
import Control.Monad (join)
import Data.List (transpose)
import Data.Set (Set)
import Data.Set qualified as Set
import Spec.TicTacToe.LocationSequence
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data MoveSequenceProperty
  = MoveSequenceValid
  | MoveSequenceContainsWin
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

splitPlayers :: [Int] -> ([Int], [Int])
splitPlayers locationSeq = go locationSeq ([], [])
  where
    go [] p = p
    go [s] (a, b) = (s : a, b)
    go (h : i : j) (a, b) = go j (h : a, i : b)

containsWin :: [Int] -> Bool
containsWin locationSeq =
  let (x, o) = splitPlayers locationSeq
   in any (all (`elem` x)) winTileSets || any (all (`elem` o)) winTileSets

winTileSets :: [Set Int]
winTileSets =
  Set.fromList
    <$> [ [0, 1, 2]
        , [3, 4, 5]
        , [6, 7, 8]
        , [0, 3, 6]
        , [1, 4, 7]
        , [2, 5, 8]
        , [0, 4, 8]
        , [2, 4, 6]
        ]

instance LogicalModel MoveSequenceProperty where
  logic = Yes

locationSequenceIsValid :: Formula (Prop LocationSequenceProperty)
locationSequenceIsValid =
  Prop
    <$> Var AllLocationsAreInBounds
    :&&: Not (Var SomeLocationIsOccupiedTwice)

instance HasLogicalModel MoveSequenceProperty [Int] where
  satisfiesProperty MoveSequenceValid ms = satisfiesExpression locationSequenceIsValid ms
  satisfiesProperty MoveSequenceContainsWin ms = containsWin ms

instance HasPermutationGenerator (Prop MoveSequenceProperty) [Int] where
  sources =
    [ Source
        { sourceName = "InvalidNoWin"
        , covers = Prop <$> Not (Var MoveSequenceValid) :&&: Not (Var MoveSequenceContainsWin)
        , gen =
            genFilter (not . containsWin) $
              genSatisfying $ Not locationSequenceIsValid
        }
    , Source
        { sourceName = "ValidNoWin"
        , covers = Prop <$> Var MoveSequenceValid :&&: Not (Var MoveSequenceContainsWin)
        , gen =
            genFilter (not . containsWin) $
              genSatisfying locationSequenceIsValid
        }
    ]
  generators =
    [ Morphism
        { name = "InvalidWin"
        , match = fmap Prop . Not $ Var MoveSequenceValid
        , contract = add (Prop MoveSequenceContainsWin)
        , morphism = \moves -> genFilter
            ( \w ->
                containsWin w
                  && satisfiesExpression (Not locationSequenceIsValid) w
            )
            $ do
              if length moves < 2
                then retry
                else do
                  winlocs <- Set.toList <$> element winTileSets
                  whofirst <- element [[moves, winlocs], [winlocs, moves]]
                  pure $ join $ transpose whofirst
        }
    , Morphism
        { name = "ValidWin"
        , match = var MoveSequenceValid
        , contract = add (Prop MoveSequenceContainsWin)
        , morphism = \moves -> do
            winlocs <- Set.toList <$> element (errLabelWhenNull "1" winTileSets)
            whofirst <- element $ errLabelWhenNull "2" [[moves, winlocs], [winlocs, moves]]
            let win = join $ transpose whofirst
            if containsWin win && satisfiesExpression locationSequenceIsValid win
              then pure win
              else retry
        }
    ]

errLabelWhenNull :: String -> [a] -> [a]
errLabelWhenNull la li = if null li then error la else li

instance HasParameterisedGenerator (Prop MoveSequenceProperty) [Int] where
  parameterisedGenerator = buildGen @(Prop MoveSequenceProperty)

moveSequencePermutationGenSelfTest :: TestTree
moveSequencePermutationGenSelfTest =
  testGroup "moveSequencePermutationGenSelfTest" $
    pure $
      fromGroup $
        permutationGeneratorSelfTest @(Prop MoveSequenceProperty)
