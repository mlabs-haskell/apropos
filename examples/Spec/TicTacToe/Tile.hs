module Spec.TicTacToe.Tile (
  TileProperty(..),
  ticTacToeTileGenSelfTests,
  ) where
import Proper.HasLogicalModel
import Proper.LogicalModel
import Proper.HasParameterisedGenerator
import Proper.HasPermutationGenerator
import Proper.HasPermutationGenerator.Contract
import Proper.HasPermutationGenerator.Gen
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (linear)
import SAT.MiniSat ( Formula (..) )
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data TileProperty =
      TileIsEmpty
    | TileIsX
    | TileIsO
    | NotATile
    | IsValidTile
    deriving stock (Eq,Ord,Enum,Show,Bounded)

instance LogicalModel TileProperty where
  logic = (ExactlyOne $ Var <$> [TileIsEmpty,TileIsX,TileIsO,NotATile])
          :&&: (ExactlyOne $ Var <$> [NotATile,IsValidTile])


instance HasLogicalModel TileProperty Integer where
  satisfiesProperty TileIsEmpty i = i == 0
  satisfiesProperty TileIsX i = i == 1
  satisfiesProperty TileIsO i = i == 2
  satisfiesProperty NotATile i = not (i `elem` [0,1,2])
  satisfiesProperty IsValidTile i = i `elem` [0,1,2]

instance HasPermutationGenerator TileProperty Integer where
  generators =
    [ PermutationEdge
      { name = "MakeTileEmpty"
      , match = Not $ Var TileIsEmpty
      , contract = clear >> addAll [IsValidTile,TileIsEmpty]
      , permuteGen = pure 0
      }
    , PermutationEdge
      { name = "MakeTileX"
      , match = Not $ Var TileIsX
      , contract = clear >> addAll [IsValidTile,TileIsX]
      , permuteGen = pure 1
      }
    , PermutationEdge
      { name = "MakeTileO"
      , match = Not $ Var TileIsO
      , contract = clear >> addAll [IsValidTile,TileIsO]
      , permuteGen = pure 2
      }
    , PermutationEdge
      { name = "MakeNotATile"
      , match = Not $ Var NotATile
      , contract = clear >> add NotATile
      , permuteGen = liftGen $ fromIntegral <$> (Gen.choice [Gen.int (linear minBound (-1))
                                                           ,Gen.int (linear 3 maxBound)])
      }

    ]

instance HasParameterisedGenerator TileProperty Integer where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen Integer
baseGen = fromIntegral <$> Gen.int (linear minBound maxBound)

ticTacToeTileGenSelfTests :: TestTree
ticTacToeTileGenSelfTests = testGroup "TicTacToe Tile permutationGeneratorSelfTest" $
  fromGroup <$> permutationGeneratorSelfTest (\(_ :: PermutationEdge TileProperty Integer) -> True) baseGen

