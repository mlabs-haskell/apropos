module Spec.TicTacToe.Board (
  BoardProperty(..),
  ticTacToeBoardGenSelfTests,
  ) where
import Spec.TicTacToe.Tile
import Proper.HasLogicalModel
import Proper.LogicalModel
import Proper.HasParameterisedGenerator
import Proper.HasPermutationGenerator
import Proper.HasPermutationGenerator.Contract
import Proper.HasPermutationGenerator.Gen
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (linear,singleton)
import SAT.MiniSat ( Formula (..) )
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import Control.Monad.Trans.Reader (ask)

data BoardProperty =
      BoardIsEmpty
    | BoardIsCorrectSize
    | BoardAllTilesValid
    | BoardHasEqualNumberOfXsAndOs
    | BoardHasOneMoreXThanO
    | BoardContainsWinForX
    | BoardContainsWinForO
    deriving stock (Eq,Ord,Enum,Show,Bounded)

instance LogicalModel BoardProperty where
  logic = Var BoardIsEmpty :->: (All $ [Var BoardAllTilesValid
                                       ,Not $ Var BoardContainsWinForX
                                       ,Not $ Var BoardContainsWinForO
                                       ,Var BoardHasEqualNumberOfXsAndOs
                                       ])

instance HasLogicalModel BoardProperty [Integer] where
  satisfiesProperty BoardIsEmpty b = sum b == 0
  satisfiesProperty BoardIsCorrectSize b = length b == 9
  satisfiesProperty BoardAllTilesValid b = all (satisfiesProperty IsValidTile) b
  satisfiesProperty BoardHasEqualNumberOfXsAndOs b =
    length (filter (==1) b) == length (filter (==2) b)
  satisfiesProperty BoardHasOneMoreXThanO b =
    length (filter (==1) b) == 1 + length (filter (==2) b)
  satisfiesProperty BoardContainsWinForX b = isWinFor 1 b
  satisfiesProperty BoardContainsWinForO b = isWinFor 2 b

isWinFor :: Eq a => a -> [a] -> Bool
isWinFor a as = any (all (==a)) (extractRows as)

-- the ways in which a board can support a double win condition
-- parameterised by length to support incorrectly sized boards
doubleWinPlaces :: Int -> [([Int],[Int])]
doubleWinPlaces l = [ (r1,r2)
                    | r1 <- rowIndices
                    , r2 <- rowIndices
                    , r1 /= r2
                    , all (<l) r1
                    , all (<l) r2
                    , not $ any (`elem` r2) r1
                    , not $ any (`elem` r1) r2]

extractRows :: [a] -> [[a]]
extractRows b =
  let l = length b
      is = filter (all (<l)) rowIndices
   in [ [b!!(i!!0),b!!(i!!1),b!!(i!!2)] | i <- is]

rowIndices :: [[Int]]
rowIndices = [[0,1,2],[3,4,5],[6,7,8] --horizontal
             ,[0,3,6],[1,4,7],[2,5,8] --vertical
             ,[0,4,8],[2,4,6]         --diagonal
             ]

setIndices :: [Int] -> [a] -> a -> [a]
setIndices [] as _ = as
setIndices (r:rs) as a =
  let h = take r as
      t = drop (r+1) as
   in setIndices rs (h <> (a:t)) a

instance HasPermutationGenerator BoardProperty [Integer] where
  generators =
    [ PermutationEdge
      { name = "SetBoardIsEmpty"
      , match = Not $ Var BoardIsEmpty
      , contract = add BoardIsEmpty >> removeAll [BoardContainsWinForX
                                                 ,BoardContainsWinForO]
      , permuteGen = do
          m <- ask
          pure $ replicate (length m) 0
      }
    , PermutationEdge
      { name = "SetBoardBoardAllTilesValid"
      , match = Not $ Var BoardAllTilesValid
      , contract = add BoardAllTilesValid >>
                   removeAll [BoardIsEmpty
                             ,BoardContainsWinForX
                             ,BoardContainsWinForO
                             ]

      , permuteGen = do
          m <- ask
          let l = max 1 (length m)
          nonempties <-  liftGen $ Gen.filter (\z -> not (isWinFor 1 z || isWinFor 2 z))
                            $ Gen.list (linear 1 l)
                            $ (fromIntegral <$> Gen.int (linear 1 2))
          let r = l - length nonempties
          empties <- list (singleton r) (genSatisfying (Var TileIsEmpty))
          liftGen $ Gen.filter (\z -> not (isWinFor 1 z || isWinFor 2 z))
                  $ Gen.shuffle $ nonempties <> empties
      }
    , PermutationEdge
      { name = "UnSetBoardBoardAllTilesValid"
      , match = Var BoardAllTilesValid
      , contract = removeAll [BoardAllTilesValid
                             ,BoardIsEmpty
                             ,BoardContainsWinForX
                             ,BoardContainsWinForO
                             ]
      , permuteGen = do
          m <- ask
          let l = max 1 (length m)
          invalids <- list (linear 1 l) (genSatisfying (Not $ Var IsValidTile))
          let r = l - length invalids
          valids <- liftGen $ Gen.filter (\z -> not (isWinFor 1 z || isWinFor 2 z))
                            $ Gen.list (singleton r)
                            $ (fromIntegral <$> Gen.int (linear 0 2))
          liftGen $ Gen.filter (\z -> not (isWinFor 1 z || isWinFor 2 z))
                  $ Gen.shuffle $ invalids <> valids
      }
    , PermutationEdge
      { name = "SetBoardIsCorrectSizeEmpty"
      , match = Not $ Var BoardIsCorrectSize
      , contract = addAll [BoardIsCorrectSize, BoardIsEmpty]
                >> removeAll [BoardContainsWinForX,BoardContainsWinForO]
      , permuteGen = pure $ replicate 9 0
      }
    , PermutationEdge
      { name = "UnsetBoardIsCorrectSizeEmpty"
      , match = Var BoardIsCorrectSize
      , contract = remove BoardIsCorrectSize >> add BoardIsEmpty
      , permuteGen = do
          l <- liftGen $ Gen.choice [Gen.int (linear 0 8), Gen.int (linear 10 100)]
          pure $ replicate l 0
      }
    , PermutationEdge
      { name = "SetBoardContainsWinForXAndO"
      , match = (Not $ Var BoardContainsWinForX) :&&: (Not $ Var BoardContainsWinForO)
      , contract = addAll [BoardContainsWinForX,BoardContainsWinForO] >> remove BoardIsEmpty
      , permuteGen = do
          m <- ask
          if satisfiesProperty BoardAllTilesValid m
             then do
               let l = max 6 (length m)
               m' <- list (singleton l) (genSatisfying (Var IsValidTile))
               (r1,r2) <- liftGen $ Gen.element $ doubleWinPlaces l
               pure $ setIndices r2 (setIndices r1 m' 1) 2
             else do
               let l = max 7 (length m)
               m' <- list (singleton l) (genSatisfying (Not $ Var IsValidTile))
               (r1,r2) <- liftGen $ Gen.element $ doubleWinPlaces l
               pure $ setIndices r2 (setIndices r1 m' 1) 2
      }
    , PermutationEdge
      { name = "SetBoardContainsWinForX"
      , match = (Not $ Var BoardContainsWinForX) :&&: (Not $ Var BoardContainsWinForO)
      , contract = add BoardContainsWinForX >> remove BoardIsEmpty
      , permuteGen = do
          m <- ask
          if satisfiesProperty BoardAllTilesValid m
             then do
               let l = max 3 (length m)
               r <- liftGen $ Gen.element $ filter (all (< l)) rowIndices
               pure $ setIndices r m 1
             else do
               let l = max 4 (length m)
               m' <- list (singleton l) (genSatisfying (Not $ Var IsValidTile))
               r <- liftGen $ Gen.element $ filter (all (< l)) rowIndices
               pure $ setIndices r m' 1
      }
    , PermutationEdge
      { name = "SetBoardContainsWinForO"
      , match = (Not $ Var BoardContainsWinForX) :&&: (Not $ Var BoardContainsWinForO)
      , contract = add BoardContainsWinForO >> remove BoardIsEmpty
      , permuteGen = do
          m <- ask
          if satisfiesProperty BoardAllTilesValid m
             then do
               let l = max 3 (length m)
               r <- liftGen $ Gen.element $ filter (all (< l)) rowIndices
               pure $ setIndices r m 2
             else do
               let l = max 4 (length m)
               m' <- list (singleton l) (genSatisfying (Not $ Var IsValidTile))
               r <- liftGen $ Gen.element $ filter (all (< l)) rowIndices
               pure $ setIndices r m' 2

      }
    ]

instance HasParameterisedGenerator BoardProperty [Integer] where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen [Integer]
baseGen = Gen.list (linear 0 100) (fromIntegral <$> Gen.int (linear minBound maxBound))

ticTacToeBoardGenSelfTests :: TestTree
ticTacToeBoardGenSelfTests = testGroup "TicTacToe Board permutationGeneratorSelfTest" $
  fromGroup <$> permutationGeneratorSelfTest (\(_ :: PermutationEdge BoardProperty [Integer] ) -> True) baseGen

