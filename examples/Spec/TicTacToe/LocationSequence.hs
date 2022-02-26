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
import qualified Data.Set as Set
import Spec.TicTacToe.Location
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data LocationSequenceProperty
  = AllLocationsAreInBounds
  | SomeLocationIsOutOfBounds
  | SomeLocationIsOccupiedTwice
  | LocationSequenceIsNull
  | LocationSequenceIsSingleton
  | LocationSequenceIsLongerThanGame
  deriving stock (Eq, Ord, Enum, Show, Bounded)

instance Enumerable LocationSequenceProperty where
  enumerated = [minBound .. maxBound]

instance LogicalModel LocationSequenceProperty where
  logic =
    (ExactlyOne $ Var <$> [AllLocationsAreInBounds, SomeLocationIsOutOfBounds])
      :&&: (Var LocationSequenceIsNull :->: Var AllLocationsAreInBounds)
      :&&: (Var LocationSequenceIsNull :->: (Not $ Var SomeLocationIsOutOfBounds))
      :&&: (Var LocationSequenceIsNull :->: (Not $ Var SomeLocationIsOccupiedTwice))
      :&&: (Var LocationSequenceIsNull :->: (Not $ Var LocationSequenceIsLongerThanGame))
      :&&: (Var LocationSequenceIsSingleton :->: (Not $ Var SomeLocationIsOccupiedTwice))
      :&&: (Var LocationSequenceIsSingleton :->: (Not $ Var LocationSequenceIsLongerThanGame))
      :&&: (AtMostOne [Var LocationSequenceIsNull, Var LocationSequenceIsSingleton])
      :&&: ( (Var LocationSequenceIsLongerThanGame :&&: Var AllLocationsAreInBounds)
              :->: Var SomeLocationIsOccupiedTwice
           )

someLocationIsOccupiedTwice :: [Int] -> Bool
someLocationIsOccupiedTwice locationSeq =
  Set.size (Set.fromList locationSeq) < length locationSeq

instance HasLogicalModel LocationSequenceProperty [Int] where
  satisfiesProperty AllLocationsAreInBounds m =
    all (satisfiesProperty LocationIsWithinBounds) m
  satisfiesProperty SomeLocationIsOutOfBounds m =
    any (satisfiesProperty LocationIsOutOfBounds) m
  satisfiesProperty SomeLocationIsOccupiedTwice m = someLocationIsOccupiedTwice m
  satisfiesProperty LocationSequenceIsNull m = length m == 0
  satisfiesProperty LocationSequenceIsSingleton m = length m == 1
  satisfiesProperty LocationSequenceIsLongerThanGame m = length m > 9

instance HasPermutationGenerator LocationSequenceProperty [Int] where
  generators =
    [ PermutationEdge
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
        , permuteGen = pure []
        }
    , PermutationEdge
        { name = "MakeAllLocationsAreInBoundsNoneOccupiedTwice"
        , match = Not $ Var LocationSequenceIsNull
        , contract =
            removeAll
              [ SomeLocationIsOutOfBounds
              , SomeLocationIsOccupiedTwice
              , LocationSequenceIsLongerThanGame
              ]
              >> add AllLocationsAreInBounds
        , permuteGen = do
            locations <- source
            let locationsLen = min 9 (length locations)
            locations' <- shuffle [0 .. 8]
            pure $ take locationsLen locations'
        }
    , PermutationEdge
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
        , permuteGen = do
            locations <- source
            let locationsLen = max 2 (length locations)
            locations' <- shuffle [0 .. 8]
            let locations'' = take (locationsLen - 1) locations'
            list (singleton locationsLen) $ element locations''
        }
    , PermutationEdge
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
        , permuteGen =
            list (singleton 1) $
              choice
                  [ int (linear minBound (-1))
                  , int (linear 9 maxBound)
                  ]
        }
    , PermutationEdge
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
        , permuteGen =
            list (singleton 1) $ int (linear 0 8)
        }
    , PermutationEdge
        { name = "MakeSomeLocationIsOutOfBoundsNoneOccupiedTwice"
        , match = Not $ Var LocationSequenceIsNull
        , contract =
            removeAll
              [ AllLocationsAreInBounds
              , SomeLocationIsOccupiedTwice
              ]
              >> add SomeLocationIsOutOfBounds
        , permuteGen =
            let f =
                  ( \m ->
                      satisfiesFormula
                        ( Var SomeLocationIsOutOfBounds
                            :&&: (Not $ Var SomeLocationIsOccupiedTwice)
                        )
                        (properties m)
                  )
             in do
                  locations <- source
                  let locationsLen = length locations
                  genFilter f $
                      list (singleton locationsLen) $
                        int (linear minBound maxBound)
        }
    , PermutationEdge
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
        , permuteGen = genFilter (satisfiesProperty SomeLocationIsOutOfBounds) $ do
            let locationsLen = 10
            locations' <- list (singleton (locationsLen - 1)) $
                  int (linear minBound maxBound)
            list (singleton locationsLen) $ element locations'
        }
    , PermutationEdge
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
        , permuteGen = genFilter (satisfiesProperty SomeLocationIsOutOfBounds) $ do
            locations <- source
            let locationsLen = length locations
            locations' <- list (singleton (locationsLen - 1)) $
                  int (linear minBound maxBound)
            list (singleton locationsLen) $ element locations'
        }
    ]

instance HasParameterisedGenerator LocationSequenceProperty [Int] where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen' [Int]
baseGen =
  let g = int (linear minBound maxBound)
   in list (linear 0 10) g

locationSequencePermutationGenSelfTest :: TestTree
locationSequencePermutationGenSelfTest =
  testGroup "locationSequencePermutationGenSelfTest" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: PermutationEdge LocationSequenceProperty [Int]) -> True)
        baseGen
