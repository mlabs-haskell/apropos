{-
Module: Spec.TicTacToe.LocationSequence
Description: TODO

-}
module Spec.TicTacToe.LocationSequence (
  LocationSequenceProperty (..),
  locationSequencePermutationGenSelfTest,
) where

import Apropos.Gen
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.HasPermutationGenerator
import Apropos.HasPermutationGenerator.Contract
import Apropos.LogicalModel
import Data.Set qualified as Set
import Spec.TicTacToe.Location
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

-- | Properties that might hold for a sequence of `Location`s.
data LocationSequenceProperty
  = -- | No locations are invalid.
    AllLocationsAreInBounds
  | -- | Some locations are invalid.
    SomeLocationIsOutOfBounds
  | -- | Some locations are duplicated.
    SomeLocationIsOccupiedTwice
  | -- | Sequence is null.
    LocationSequenceIsNull
  | -- | Sequence consists of a single step.
    LocationSequenceIsSingleton
  | -- | Sequence is longer than nine elements.
    LocationSequenceIsLongerThanGame
  deriving stock (Eq, Ord, Enum, Show, Bounded)

instance Enumerable LocationSequenceProperty where
  enumerated = [minBound .. maxBound]

instance LogicalModel LocationSequenceProperty where
  logic =
    -- It cannot be true that all locations are in bounds but some
    -- are out of bounds.
    ExactlyOne (Var <$> [AllLocationsAreInBounds, SomeLocationIsOutOfBounds])
      -- An empty sequence is considered to be wholly in-bounds.
      :&&: (Var LocationSequenceIsNull :->: Var AllLocationsAreInBounds)
      :&&: (Var LocationSequenceIsNull :->: Not (Var SomeLocationIsOutOfBounds))
      -- An empty sequence cannot have multiply-occupied locations.
      :&&: (Var LocationSequenceIsNull :->: Not (Var SomeLocationIsOccupiedTwice))
      -- An empty sequence is not longer than nine moves.
      :&&: (Var LocationSequenceIsNull :->: Not (Var LocationSequenceIsLongerThanGame))
      -- A singleton sequence does not allow for doubly-occupied squares.
      :&&: (Var LocationSequenceIsSingleton :->: Not (Var SomeLocationIsOccupiedTwice))
      -- A singleton sequence has a length fewer than nine.
      :&&: (Var LocationSequenceIsSingleton :->: Not (Var LocationSequenceIsLongerThanGame))
      -- A sequence can be either null or a singleton but not both.
      :&&: AtMostOne [Var LocationSequenceIsNull, Var LocationSequenceIsSingleton]
      -- If a sequence is longer than nine steps but all locations
      -- are within bounds, some squares are necessarily
      -- doubly-occupied.
      :&&: ( (Var LocationSequenceIsLongerThanGame :&&: Var AllLocationsAreInBounds)
              :->: Var SomeLocationIsOccupiedTwice
           )

{- | Returns `True` if duplicate locations are present in list,
     `False` otherwise.
-}
someLocationIsOccupiedTwice :: [Int] -> Bool
someLocationIsOccupiedTwice locationSeq =
  Set.size (Set.fromList locationSeq) < length locationSeq

instance HasLogicalModel LocationSequenceProperty [Int] where
  satisfiesProperty AllLocationsAreInBounds m =
    all (satisfiesProperty LocationIsWithinBounds) m
  satisfiesProperty SomeLocationIsOutOfBounds m =
    any (satisfiesProperty LocationIsOutOfBounds) m
  satisfiesProperty SomeLocationIsOccupiedTwice m = someLocationIsOccupiedTwice m
  satisfiesProperty LocationSequenceIsNull m = null m
  satisfiesProperty LocationSequenceIsSingleton m = length m == 1
  satisfiesProperty LocationSequenceIsLongerThanGame m = length m > 9

instance HasPermutationGenerator LocationSequenceProperty [Int] where
  generators =
    [ Morphism
        { name = "MakeLocationSequenceNull"
        , match = Yes
        , contract =
            removeAll
              [ SomeLocationIsOutOfBounds
              , SomeLocationIsOccupiedTwice
              , LocationSequenceIsSingleton
              , LocationSequenceIsLongerThanGame
              ]
              >> addAll [AllLocationsAreInBounds, LocationSequenceIsNull]
        , morphism = \_ -> pure []
        }
    , Morphism
        { name = "MakeAllLocationsAreInBoundsNoneOccupiedTwice"
        , match = Not $ Var LocationSequenceIsNull
        , contract =
            removeAll
              [ SomeLocationIsOutOfBounds
              , SomeLocationIsOccupiedTwice
              , LocationSequenceIsLongerThanGame
              ]
              >> add AllLocationsAreInBounds
        , morphism = \locations -> do
            let locationsLen = min 9 (length locations)
            locations' <- shuffle [0 .. 8]
            pure $ take locationsLen locations'
        }
    , Morphism
        { name = "MakeAllLocationsAreInBoundsSomeOccupiedTwice"
        , match = Yes
        , contract =
            removeAll
              [ SomeLocationIsOutOfBounds
              , LocationSequenceIsNull
              ]
              >> addAll
                [ AllLocationsAreInBounds
                , SomeLocationIsOccupiedTwice
                ]
        , morphism = \locations -> do
            let locationsLen = max 2 (length locations)
            locations' <- shuffle [0 .. 8]
            let locations'' = take (locationsLen - 1) locations'
            list (singleton locationsLen) $ element locations''
        }
    , Morphism
        { name = "MakeOutOfBoundsSingleton"
        , match = Yes
        , contract =
            removeAll
              [ AllLocationsAreInBounds
              , SomeLocationIsOccupiedTwice
              , LocationSequenceIsNull
              , LocationSequenceIsLongerThanGame
              ]
              >> addAll [SomeLocationIsOutOfBounds, LocationSequenceIsSingleton]
        , morphism = \_ ->
            list (singleton 1) $
              choice
                [ int (linear minBound (-1))
                , int (linear 9 maxBound)
                ]
        }
    , Morphism
        { name = "MakeInBoundsSingleton"
        , match = Yes
        , contract =
            removeAll
              [ SomeLocationIsOutOfBounds
              , SomeLocationIsOccupiedTwice
              , LocationSequenceIsNull
              , LocationSequenceIsLongerThanGame
              ]
              >> addAll [AllLocationsAreInBounds, LocationSequenceIsSingleton]
        , morphism = \_ -> list (singleton 1) $ int (linear 0 8)
        }
    , Morphism
        { name = "MakeSomeLocationIsOutOfBoundsNoneOccupiedTwice"
        , match = Not $ Var LocationSequenceIsNull
        , contract =
            removeAll
              [ AllLocationsAreInBounds
              , SomeLocationIsOccupiedTwice
              ]
              >> add SomeLocationIsOutOfBounds
        , morphism = \locations ->
            let f =
                  ( satisfiesFormula
                      ( Var SomeLocationIsOutOfBounds
                          :&&: Not (Var SomeLocationIsOccupiedTwice)
                      )
                      . properties
                  )
             in do
                  let locationsLen = length locations
                  genFilter f $
                    list (singleton locationsLen) $
                      int (linear minBound maxBound)
        }
    , Morphism
        { name = "MakeSomeLocationIsOccupiedTwiceSequenceTooLong"
        , match = Not (Var LocationSequenceIsNull :||: Var LocationSequenceIsSingleton)
        , contract =
            removeAll
              [ AllLocationsAreInBounds
              , LocationSequenceIsNull
              , LocationSequenceIsSingleton
              ]
              >> addAll
                [ SomeLocationIsOccupiedTwice
                , SomeLocationIsOutOfBounds
                , LocationSequenceIsLongerThanGame
                ]
        , morphism = \_ -> genFilter (satisfiesProperty SomeLocationIsOutOfBounds) $ do
            let locationsLen = 10
            locations' <-
              list (singleton (locationsLen - 1)) $
                int (linear minBound maxBound)
            list (singleton locationsLen) $ element locations'
        }
    , Morphism
        { name = "MakeSomeLocationIsOccupiedTwice"
        , match = Not (Var LocationSequenceIsNull :||: Var LocationSequenceIsSingleton)
        , contract =
            removeAll
              [ AllLocationsAreInBounds
              , LocationSequenceIsNull
              , LocationSequenceIsSingleton
              ]
              >> addAll
                [ SomeLocationIsOccupiedTwice
                , SomeLocationIsOutOfBounds
                ]
        , morphism = \locations -> genFilter (satisfiesProperty SomeLocationIsOutOfBounds) $ do
            let locationsLen = length locations
            locations' <-
              list (singleton (locationsLen - 1)) $
                int (linear minBound maxBound)
            list (singleton locationsLen) $ element locations'
        }
    ]

instance HasParameterisedGenerator LocationSequenceProperty [Int] where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen [Int]
baseGen =
  let g = int (linear minBound maxBound)
   in list (linear 0 10) g

locationSequencePermutationGenSelfTest :: TestTree
locationSequencePermutationGenSelfTest =
  testGroup "locationSequencePermutationGenSelfTest" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: Morphism LocationSequenceProperty [Int]) -> True)
        baseGen
