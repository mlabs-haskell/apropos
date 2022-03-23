{-
Module: Spec.TicTacToe.Move
Description: Logical properties of a TicTacToe move.

-}
module Spec.TicTacToe.Move (
  MoveProperty (..),
  movePermutationGenSelfTest,
) where

import Apropos.Gen
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.HasPermutationGenerator
import Apropos.LogicalModel
import Control.Lens.Tuple (_1, _2)
import Control.Monad (join)
import Data.Foldable (find)
import Data.Set (Set)
import Data.Set qualified as Set (singleton)
import GHC.Generics (Generic)
import Spec.TicTacToe.Location
import Spec.TicTacToe.Player
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)


{- | A TicTacToe move may be modelled as a pair of a `Player` and
     a `Location`.
-}
type MoveModel = (Player, Location)

-- | Collection of properties that a TicTacToe move might have.
data MoveProperty
  = -- | The location where the mark should be placed.
    MoveLocation LocationProperty
  | -- | The mark of the player i.e. noughts or crosses.
    MovePlayer PlayerProperty
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable)

{- | Any logical move consists of the location the mark is being
     placed and the mark a player is using.
-}
instance LogicalModel MoveProperty where
  logic :: Formula MoveProperty
  logic = (MoveLocation <$> logic) :&&: (MovePlayer <$> logic)

{- | For `MoveLocation` properties, the logic is the same
     as `satisfiesProperty` called on the `LocationProperty` and
     the `Location` value.

     For `MovePlayer` properties, the logic is the same as
     `satisfiesProperty` called on the `PlayerProperty` and
     the `Player` value.
-}
instance HasLogicalModel MoveProperty MoveModel where
  satisfiesProperty :: MoveProperty -> MoveModel -> Bool
  satisfiesProperty (MoveLocation prop) (_, location) = satisfiesProperty prop location
  satisfiesProperty (MovePlayer prop) (player, _) = satisfiesProperty prop player

instance HasPermutationGenerator MoveProperty MoveModel where
  generators :: [Morphism MoveProperty MoveModel]
  generators =
    {-
    TODO: What on Earth are `Abstraction`s?

    -}
    let l =
          Abstraction
            { abstractionName = "MovePlayer"
            , propertyAbstraction = abstractsProperties MovePlayer
            , modelAbstraction = _1
            }
        r =
          Abstraction
            { abstractionName = "MoveLocation"
            , propertyAbstraction = abstractsProperties MoveLocation
            , modelAbstraction = _2
            }
     in join -- join :: Monad m => m (m a) -> m a
          [ abstract -- abstract ::
              l
              <$> generators
          , abstract r
              <$> generators
          ]

instance HasParameterisedGenerator MoveProperty MoveModel where
  parameterisedGenerator :: Set MoveProperty -> Gen MoveModel
  parameterisedGenerator mPropSet = do
    let locProp :: LocationProperty
        locProp = case find isMoveLoc mPropSet of
          Just (MoveLocation lP) -> lP
          _ -> error "MoveLocation not present in property set."

        playProp :: PlayerProperty
        playProp = case find isMovePlay mPropSet of
          Just (MovePlayer pP) -> pP
          _ -> error "MovePlayer not present in property set."
    location <- parameterisedGenerator $ Set.singleton locProp
    player <- parameterisedGenerator $ Set.singleton playProp
    return (location, player)
    where
      isMoveLoc :: MoveProperty -> Bool
      isMoveLoc (MoveLocation _) = True
      isMoveLoc _ = False

      isMovePlay :: MoveProperty -> Bool
      isMovePlay (MovePlayer _) = True
      isMovePlay _ = False

baseGen :: Gen (Int, Int)
baseGen =
  let -- g is a generator for any `Int` from `minBound` to `maxBound`.
      g :: Gen Int
      g = int (linear minBound maxBound)

      g' :: Gen (Int -> (Int, Int))
      g' = fmap (,) g
   in g' <*> g

movePermutationGenSelfTest :: TestTree
movePermutationGenSelfTest =
  testGroup "movePermutationGenSelfTest" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: Morphism MoveProperty (Int, Int)) -> True)
        baseGen
