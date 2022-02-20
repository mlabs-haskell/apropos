module Spec.TicTacToe.Board (
  BoardProperty(..),
  ticTacToeBoardGenSelfTests,
  ) where
import Spec.TicTacToe.Tile
import Proper.HasLogicalModel
import Proper.LogicalModel
import Proper.HasParameterisedGenerator
import Proper.HasPermutationGenerator
--import Proper.HasPermutationGenerator.Contract
import Proper.HasPermutationGenerator.Gen
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (linear) --,singleton)
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


instance HasLogicalModel BoardProperty [Integer] where
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

instance HasPermutationGenerator BoardProperty [Integer] where
  generators =
    [ PermutationEdge
      { name = ""
      , match = Yes
      , contract = pure ()
      , permuteGen = ask
      }
    ]

instance HasParameterisedGenerator BoardProperty [Integer] where
  parameterisedGenerator = buildGen baseGen

baseGen :: PGen [Integer]
baseGen = liftGenP $ Gen.list (linear 0 100) (fromIntegral <$> Gen.int (linear minBound maxBound))

ticTacToeBoardGenSelfTests :: TestTree
ticTacToeBoardGenSelfTests = testGroup "TicTacToe Board permutationGeneratorSelfTest" $
  fromGroup <$> permutationGeneratorSelfTest (\(_ :: PermutationEdge BoardProperty [Integer] ) -> True) baseGen

