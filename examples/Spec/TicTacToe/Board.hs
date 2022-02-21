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
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (linear,singleton)
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import Control.Monad.Trans.Reader (ask)

data BoardProperty =
      BoardIsCorrectSize
    | BoardAllTilesValid
    | BoardFull
    | BoardHasEqualNumberOfXsAndOs
    | BoardHasOneMoreXThanO
    | BoardContainsWinForX
    | BoardContainsWinForO
    deriving stock (Eq,Ord,Enum,Show,Bounded)

instance Enumerable BoardProperty where
  enumerated = [minBound..maxBound]

instance LogicalModel BoardProperty where
  logic = All [AtMostOne $ Var <$> [BoardHasEqualNumberOfXsAndOs,BoardHasOneMoreXThanO]
              ,(Var BoardFull :&&: Var BoardIsCorrectSize :&&: Var BoardAllTilesValid)
                :->: Not (Var BoardHasEqualNumberOfXsAndOs)
              ]


instance HasLogicalModel BoardProperty [Int] where
  satisfiesProperty BoardIsCorrectSize b = length b == 9
  satisfiesProperty BoardAllTilesValid b = all (satisfiesProperty IsValidTile) b
  satisfiesProperty BoardFull b = not $ any (==0) b
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
_doubleWinPlaces :: Int -> [([Int],[Int])]
_doubleWinPlaces l = [ (r1,r2)
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

_setIndices :: [Int] -> [a] -> a -> [a]
_setIndices [] as _ = as
_setIndices (r:rs) as a =
  let h = take r as
      t = drop (r+1) as
   in _setIndices rs (h <> (a:t)) a

zipReplace :: (a -> Bool) -> [a] -> [a] -> [a]
zipReplace _ _ [] = []
zipReplace _ [] es = es
zipReplace c (h:r) (e:es) | c e = h:(zipReplace c r es)
                          | otherwise = e:(zipReplace c (h:r) es)

instance HasPermutationGenerator BoardProperty [Int] where
  generators =
    [ PermutationEdge
      { name = ""
      , match = Yes
      , contract = clear
      , permuteGen = do
          boardSize <- liftGenPA $ Gen.choice $ Gen.int <$> [linear 3 8,linear 10 100]
          invalidsSize <- liftGenPA $ Gen.int (linear 1 (boardSize-2))
          invalids <- list (singleton invalidsSize) $ genSatisfying (Var NotATile)
          emptiesSize <- liftGenPA $ Gen.int (linear 1 (boardSize - invalidsSize - 1))
          let empties = replicate emptiesSize 0
          let xosSize = boardSize - invalidsSize - emptiesSize
          xos <- liftGenPA $ Gen.filter (not . satisfiesAny [BoardHasEqualNumberOfXsAndOs
                                                            ,BoardHasOneMoreXThanO])
                           $ Gen.list (singleton xosSize)
                           $ Gen.int (linear 1 2)
          liftGenPA $ Gen.filter (not . satisfiesAny [BoardContainsWinForX
                                                     ,BoardContainsWinForO])
                    $ Gen.shuffle (invalids <> empties <> xos)
      }
    , PermutationEdge
      { name = "SetBoardFullNotAllValid"
      , match = Not $ Var BoardAllTilesValid
      , contract = add BoardFull
      , permuteGen = do
          b <- ask
          let numEmpty = length $ filter (== 0) b
          invalids <- list (singleton numEmpty) $ genSatisfying (Not $ Var TileIsEmpty)
          pure $ zipReplace (==0) invalids b

      }
    , PermutationEdge
      { name = "EmptyCorrectSizedBoard"
      , match = Yes
      , contract = clear >> addAll [BoardIsCorrectSize
                                   ,BoardAllTilesValid
                                   ,BoardHasEqualNumberOfXsAndOs
                                   ]
      , permuteGen = pure $ replicate 9 0
      }
    , PermutationEdge
      { name = "EmptyIncorrectSizedBoard"
      , match = Yes
      , contract = clear >> addAll [BoardAllTilesValid
                                   ,BoardHasEqualNumberOfXsAndOs
                                   ]
      , permuteGen = do
          boardSize <- liftGenPA $ Gen.choice $ Gen.int <$> [linear 0 8,linear 10 100]
          pure $ replicate boardSize 0
      }
    , PermutationEdge
      { name = "FullIncorrectSizedBoardNoneValid"
      , match = Yes
      , contract = clear >> addAll [BoardFull
                                   ,BoardHasEqualNumberOfXsAndOs]
      , permuteGen = do
          boardSize <- liftGenPA $ Gen.choice $ Gen.int <$> [linear 1 8
                                                            ,linear 10 100
                                                            ]
          list (singleton boardSize) $ genSatisfying (Var NotATile)
      }
    , PermutationEdge
      { name = "IncorrectSizedBoardNoneValid"
      , match = Yes
      , contract = clear >> addAll [BoardHasEqualNumberOfXsAndOs]
      , permuteGen = do
          boardSize <- liftGenPA $ Gen.choice $ Gen.int <$> [linear 2 8
                                                            ,linear 10 100
                                                            ]
          invalidsSize <- liftGenPA $ Gen.int (linear 1 (boardSize-1))
          nats <- list (singleton invalidsSize) $ genSatisfying (Var NotATile)
          emps <- list (singleton (boardSize-invalidsSize))
                      $ genSatisfying (Var TileIsEmpty)
          liftGenPA $ Gen.shuffle (nats <> emps)
      }
    , PermutationEdge
      { name = "IncorrectSizedBoardNoneValidOneMoreXThanO"
      , match = Yes
      , contract = clear >> addAll [BoardHasOneMoreXThanO]
      , permuteGen = do
          boardSize <- liftGenPA $ Gen.choice $ Gen.int <$> [linear 3 8,linear 10 100]
          xosSize <- liftGenPA $ Gen.filter odd $ Gen.int (linear 1 (boardSize-2))
          xos <- liftGenPA $ Gen.filter (satisfiesProperty BoardHasOneMoreXThanO)
                           $ Gen.list (singleton xosSize)
                           $ Gen.int (linear 1 2)
          emptiesSize <- liftGenPA $ Gen.int (linear 1 (boardSize - xosSize - 1))
          let empties = replicate emptiesSize 0
          let invalidsSize = boardSize - xosSize - emptiesSize
          invalids <- list (singleton invalidsSize) $ genSatisfying (Var NotATile)
          liftGenPA $ Gen.filter (not . satisfiesAny [BoardContainsWinForX
                                                     ,BoardContainsWinForO])
                    $ Gen.shuffle (xos <> empties <> invalids)
      }
    , PermutationEdge
      { name = "SetBoardContainsWinForX"
      , match = Yes
      , contract = add BoardContainsWinForX
      , permuteGen = ask
      }
    , PermutationEdge
      { name = "SetBoardContainsWinForO"
      , match = Yes
      , contract = add BoardContainsWinForO
      , permuteGen = ask
      }
    , PermutationEdge
      { name = "SetBoardIsCorrectSize"
      , match = Yes
      , contract = add BoardIsCorrectSize
      , permuteGen = ask
      }
    , PermutationEdge
      { name = "SetBoardAllTilesValid"
      , match = Yes
      , contract = add BoardAllTilesValid
      , permuteGen = ask
      }
    , PermutationEdge
      { name = "SetBoardHasOneMoreXThanO"
      , match = Yes
      , contract = add BoardHasOneMoreXThanO
      , permuteGen = ask
      }
    ]

instance HasParameterisedGenerator BoardProperty [Int] where
  parameterisedGenerator = buildGen baseGen

baseGen :: PGen [Int]
baseGen = liftGenP $ Gen.list (linear 0 100) (fromIntegral <$> Gen.int (linear minBound maxBound))

ticTacToeBoardGenSelfTests :: TestTree
ticTacToeBoardGenSelfTests = testGroup "TicTacToe Board permutationGeneratorSelfTest" $
  fromGroup <$> permutationGeneratorSelfTest (\(_ :: PermutationEdge BoardProperty [Int] ) -> True) baseGen

