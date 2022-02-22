module Spec.TicTacToe.LocationSequence (
  LocationSequenceProperty(..),
  locationSequencePermutationGenSelfTest,
  ) where
import Spec.TicTacToe.Location
import Brutus.HasLogicalModel
import Brutus.LogicalModel
import Brutus.HasParameterisedGenerator
import Brutus.HasPermutationGenerator
import Brutus.HasPermutationGenerator.Contract
import Brutus.Gen
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (linear,singleton)
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import qualified Data.Set as Set

data LocationSequenceProperty =
    AllLocationsAreInBounds
  | SomeLocationIsOutOfBounds
  | SomeLocationIsOccupiedTwice
  | LocationSequenceIsNull
  | LocationSequenceIsSingleton
  deriving stock (Eq,Ord,Enum,Show,Bounded)

instance Enumerable LocationSequenceProperty where
  enumerated = [minBound..maxBound]

instance LogicalModel LocationSequenceProperty where
  logic = (ExactlyOne $ Var <$> [AllLocationsAreInBounds,SomeLocationIsOutOfBounds])
       :&&: (Var LocationSequenceIsNull :->: Var AllLocationsAreInBounds)
       :&&: (Var LocationSequenceIsNull :->: (Not $ Var SomeLocationIsOutOfBounds))
       :&&: (Var LocationSequenceIsNull :->: (Not  $ Var SomeLocationIsOccupiedTwice))
       :&&: (Var LocationSequenceIsSingleton :->: (Not $ Var SomeLocationIsOccupiedTwice))
       :&&: (AtMostOne [Var LocationSequenceIsNull,Var LocationSequenceIsSingleton])


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

instance HasPermutationGenerator LocationSequenceProperty [Int] where
  generators =
    [  PermutationEdge
      { name = "MakeLocationSequenceNull"
      , match = Yes
      , contract = removeAll [SomeLocationIsOutOfBounds
                             ,SomeLocationIsOccupiedTwice
                             ,LocationSequenceIsSingleton]
                >> addAll [AllLocationsAreInBounds,LocationSequenceIsNull]
      , permuteGen = pure []
      }
    , PermutationEdge
      { name = "MakeAllLocationsAreInBoundsNoneOccupiedTwice"
      , match = Not $ Var LocationSequenceIsNull
      , contract = removeAll [SomeLocationIsOutOfBounds
                             ,SomeLocationIsOccupiedTwice
                             ]
                >> add AllLocationsAreInBounds
      , permuteGen = do
          locations <- source
          let locationsLen = min 9 (length locations)
          locations' <- liftGenPA $ Gen.shuffle [0..8]
          pure $ take locationsLen locations'
      }
    , PermutationEdge
      { name = "MakeAllLocationsAreInBoundsSomeOccupiedTwice"
      , match = Yes
      , contract = removeAll [SomeLocationIsOutOfBounds
                             ,LocationSequenceIsNull
                             ]
                >> addAll [AllLocationsAreInBounds
                          ,SomeLocationIsOccupiedTwice
                          ]
      , permuteGen = do
          locations <- source
          let locationsLen = max 2 (length locations)
          locations' <- liftGenPA $ Gen.shuffle [0..8]
          let locations'' = take (locationsLen - 1) locations'
          liftGenPA $ Gen.list (singleton locationsLen) $ Gen.element locations''
      }
    , PermutationEdge
      { name = "MakeOutOfBoundsSingleton"
      , match = Yes
      , contract = removeAll [AllLocationsAreInBounds
                             ,SomeLocationIsOccupiedTwice
                             ,LocationSequenceIsNull]
                >> addAll [SomeLocationIsOutOfBounds,LocationSequenceIsSingleton]
      , permuteGen =
          liftGenPA $ Gen.list (singleton 1)
                    $ Gen.choice [Gen.int (linear minBound (-1))
                                 ,Gen.int (linear 9 maxBound)
                                 ]
      }
    , PermutationEdge
      { name = "MakeInBoundsSingleton"
      , match = Yes
      , contract =  removeAll [SomeLocationIsOutOfBounds
                             ,SomeLocationIsOccupiedTwice
                             ,LocationSequenceIsNull]
                >> addAll [AllLocationsAreInBounds,LocationSequenceIsSingleton]
      , permuteGen =
          liftGenPA $ Gen.list (singleton 1)
                    $ Gen.int (linear 0 8)
      }
    , PermutationEdge
      { name = "MakeSomeLocationIsOutOfBoundsNoneOccupiedTwice"
      , match = Not $ Var LocationSequenceIsNull
      , contract = removeAll [AllLocationsAreInBounds
                             ,SomeLocationIsOccupiedTwice
                             ]
                >> add SomeLocationIsOutOfBounds
      , permuteGen =
          let f = (\m -> satisfiesFormula (Var SomeLocationIsOutOfBounds
                    :&&: (Not $ Var SomeLocationIsOccupiedTwice)) (properties m))
            in do
                 locations <- source
                 let locationsLen = length locations
                 liftGenPA $ Gen.filter f
                           $ Gen.list (singleton locationsLen)
                           $ Gen.int (linear minBound maxBound)
      }
    , PermutationEdge
      { name = "MakeSomeLocationIsOccupiedTwice"
      , match = Not (Var LocationSequenceIsNull :||: Var LocationSequenceIsSingleton)
      , contract = removeAll [AllLocationsAreInBounds
                             ,LocationSequenceIsNull
                             ,LocationSequenceIsSingleton
                             ]
                >> addAll [SomeLocationIsOccupiedTwice
                          ,SomeLocationIsOutOfBounds
                          ]
      , permuteGen = filterPA (satisfiesProperty SomeLocationIsOutOfBounds) $ do
          locations <- source
          let locationsLen = length locations
          locations' <- liftGenPA $ Gen.list (singleton (locationsLen - 1))
                      $ Gen.int (linear minBound maxBound)
          liftGenPA $ Gen.list (singleton locationsLen) $ Gen.element locations'
      }
    ]

instance HasParameterisedGenerator LocationSequenceProperty [Int] where
  parameterisedGenerator = buildGen baseGen

baseGen :: PGen [Int]
baseGen =
  let g = Gen.int (linear minBound maxBound)
   in liftGenP $ Gen.list (linear 0 10) g

locationSequencePermutationGenSelfTest :: TestTree
locationSequencePermutationGenSelfTest = testGroup "movePermutationGenSelfTest" $
  fromGroup <$> permutationGeneratorSelfTest True
             (\(_ :: PermutationEdge LocationSequenceProperty [Int]) -> True)
             baseGen


