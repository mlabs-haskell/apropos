{-# LANGUAGE TemplateHaskell #-}

module Spec.TicTacToe (ticTacToeGenTests,ticTacToeGenSelfTests) where
import Spec.TicTacToe.Board
import Proper.LogicalModel
import Proper.HasLogicalModel
import Proper.HasParameterisedGenerator
import Proper.HasPermutationGenerator
import Proper.HasPermutationGenerator.Contract
import Proper.HasPermutationGenerator.Gen
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (linear)
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import Data.Proxy (Proxy(..))
import Control.Monad.Trans.Reader (ask)
import Control.Monad (join)

data TicTacToeMove =
  TicTacToeMove {
    from :: [Int]
  , to   :: [Int]
  , player :: Int
  , declare :: Bool
  } deriving stock (Show)

-- It is tempting (and legal) to write this but it will take an age to run.
--
--data TicTacToeProperty =
--      FromBoardProperty BoardProperty
--    | ToBoardProperty BoardProperty
--    | ...
--
-- Here's why.
--
-- Model complexity!
--
-- Lifting the logic from board which has 92 possible combinations of properties
-- results in a huge model with 8464 possible combinations before we start adding
-- the game play logic which would only multiply that number further.
--
-- We can still reuse the model for board when constructing our properties and
-- generators however which will give us more elegant property definitions as well as
-- powerful control over the distributional properties of the board generator.
--
-- Using genSatisfying will give us uniform higher order randomness over the distributions
-- for each set of satisfying model properties which means edge case scenarios are less
-- likely to go unnoticed as they are prone to do with hand rolled generators.

data TicTacToeProperty =
      PlayerIsX
    | PlayerIsO
    | IsPlayersTurn
    | WinDeclared
    | IsValidWinDeclaration
    | PlacementValid
    | IsValidMove
    deriving stock (Eq,Ord,Show)

$(gen_enumerable ''TicTacToeProperty)

instance LogicalModel TicTacToeProperty where
  logic = All [ Var PlayerIsX :->: (Not $ Var PlayerIsO)
              , Var PlayerIsO :->: (Not $ Var PlayerIsX)
              , Var IsValidMove :->: (All $ Var <$> [PlacementValid,IsPlayersTurn])
              , (Not $ Var PlacementValid) :->: (Not $ Var IsValidMove)
              , (Not $ Var IsPlayersTurn) :->: (Not $ Var IsValidMove)
              ]

instance HasLogicalModel TicTacToeProperty TicTacToeMove where
  satisfiesProperty PlayerIsX m = 1 == player m
  satisfiesProperty PlayerIsO m = 2 == player m
  satisfiesProperty IsPlayersTurn m =
    case player m of
      1 -> satisfiesProperty BoardHasEqualNumberOfXsAndOs (from m)
      2 -> satisfiesProperty BoardHasOneMoreXThanO (from m)
      _ -> False
  satisfiesProperty WinDeclared m = declare m
  satisfiesProperty IsValidWinDeclaration m =
    if satisfiesProperty BoardContainsWinForX (from m)
       || satisfiesProperty BoardContainsWinForO (from m)
       then False
       else if satisfiesProperty BoardContainsWinForX (to m)
              then satisfiesProperty PlayerIsX m && satisfiesProperty WinDeclared m
              else if satisfiesProperty BoardContainsWinForO (to m)
                     then satisfiesProperty PlayerIsO m && satisfiesProperty WinDeclared m
                     else False
  satisfiesProperty IsValidMove  m =
    satisfiesProperty IsPlayersTurn m &&
      if satisfiesProperty PlayerIsX m
         then satisfiesProperty BoardHasOneMoreXThanO (to m)
           && satisfiesProperty PlacementValid m
         else if satisfiesProperty PlayerIsO m
                then satisfiesProperty BoardHasEqualNumberOfXsAndOs (to m)
                  && satisfiesProperty PlacementValid m
                else False
  satisfiesProperty PlacementValid m =
    validDiff (from m) (to m)
    && satisfiesFormula ((None $ Var <$> [BoardContainsWinForX
                                         ,BoardContainsWinForO
                                         ,BoardFull
                                         ])
                    :&&: (All $ Var <$> [BoardIsCorrectSize
                                        ,BoardAllTilesValid
                                        ])) (properties $ from m)
    && satisfiesFormula (All $ Var <$> [BoardIsCorrectSize
                                       ,BoardAllTilesValid
                                       ]) (properties $ to m)

validDiff :: [Int] -> [Int] -> Bool
validDiff f t = 
  let diffs = filter (\(l,r) -> l /= r) $ zip f t
      adds = filter added diffs
   in ((length diffs) == 1) && ((length adds) == 1)
  where
    added :: (Int,Int) -> Bool
    added (0,i) | i /= 0 = True
    added _ = False

instance HasPermutationGenerator TicTacToeProperty TicTacToeMove where
  generators =
    [ PermutationEdge
      { name = "SetPlayerX"
      , match = Not $ Var PlayerIsX
      , contract = remove PlayerIsO >> add PlayerIsX
      , permuteGen = do
          m <- ask
          pure $ m { player = 1 }
      }
    , PermutationEdge
      { name = "SetPlayerO"
      , match = Not $ Var PlayerIsO
      , contract = remove PlayerIsX >> add PlayerIsO
      , permuteGen = do
          m <- ask
          pure $ m { player = 2 }
      }
    , PermutationEdge
      { name = "SetInvalidPlayer"
      , match = Var PlayerIsO :||: Var PlayerIsX
      , contract = remove PlayerIsX >> remove PlayerIsO
      , permuteGen = do
          m <- ask
          i <- liftGenPA $ Gen.choice $ (Gen.int <$> [ (linear minBound 0)
                                                     , (linear 3 maxBound)
                                                     ])
          pure $ m { player = i }
      }
    , PermutationEdge
      { name = "SetWinDeclared"
      , match = Not $ Var WinDeclared
      , contract = add WinDeclared
      , permuteGen = do
          m <- ask
          pure $ m { declare = True }
      }
    , PermutationEdge
      { name = "UnsetWinDeclared"
      , match = Var WinDeclared
      , contract = remove WinDeclared
      , permuteGen = do
          m <- ask
          pure $ m { declare = False }
      }
    , PermutationEdge
      { name = "SetIsPlayersTurn"
      , match = Not $ Var IsPlayersTurn
      , contract = add IsPlayersTurn
      , permuteGen = ask --TODO
      }
    , PermutationEdge
      { name = "UnsetIsPlayersTurn"
      , match = Var IsPlayersTurn
      , contract = remove IsPlayersTurn
      , permuteGen = ask --TODO
      }
    , PermutationEdge
      { name = "SetIsValidWinDeclaration"
      , match = Not $ Var IsValidWinDeclaration
      , contract = add IsValidWinDeclaration
      , permuteGen = ask --TODO
      }
    , PermutationEdge
      { name = "UnsetIsValidWinDeclaration"
      , match = Var IsValidWinDeclaration
      , contract = remove IsValidWinDeclaration
      , permuteGen = ask --TODO
      }
    , PermutationEdge
      { name = "SetPlacementValid"
      , match = Not $ Var PlacementValid
      , contract = add PlacementValid
      , permuteGen = ask --TODO
      }
    , PermutationEdge
      { name = "UnsetPlacementValid"
      , match = Var PlacementValid
      , contract = remove PlacementValid
      , permuteGen = ask --TODO
      }
    , PermutationEdge
      { name = "SetIsValidMove"
      , match = Not $ Var IsValidMove
      , contract = add IsValidMove
      , permuteGen = ask --TODO
      }
    , PermutationEdge
      { name = "UnsetIsValidMove"
      , match = Var IsValidMove
      , contract = remove IsValidMove
      , permuteGen = ask --TODO
      }

    ]

instance HasParameterisedGenerator TicTacToeProperty TicTacToeMove where
  parameterisedGenerator = buildGen baseGen

baseGen :: PGen TicTacToeMove
baseGen = do
    let genTile = liftGenP (Gen.int (linear minBound maxBound))
        genBoard = genSatisfying (Yes :: Formula BoardProperty)
        genPlayer = genTile
        genDeclare = liftGenP $ Gen.bool
    TicTacToeMove <$> genBoard
                  <*> genBoard
                  <*> genPlayer
                  <*> genDeclare

ticTacToeGenSelfTests :: TestTree
ticTacToeGenSelfTests = testGroup "TicTacToe HasPermutationGenerator permutationGeneratorSelfTest" $
  fromGroup <$> permutationGeneratorSelfTest (\(_ :: PermutationEdge TicTacToeProperty TicTacToeMove) -> True) baseGen

ticTacToeGenTests :: TestTree
ticTacToeGenTests = testGroup "Spec TicTacToe" $
    fromGroup <$> [
      runGeneratorTestsWhere (Proxy :: Proxy TicTacToeMove)
                             "TicTacToe Generator"
                             (Yes :: Formula TicTacToeProperty)
    ]

