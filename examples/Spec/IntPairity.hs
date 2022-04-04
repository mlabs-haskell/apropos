{-# LANGUAGE MultiWayIf #-}

module Spec.IntPairity (
  intPairityGenSelfTests,
  switchPairity,
  Pairity (..),
) where

import Apropos
import GHC.Generics
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data Pairity
  = Even
  | Odd
  deriving stock (Eq, Ord, Enum, Show, Bounded, Generic)
  deriving anyclass (Enumerable)

instance LogicalModel Pairity where
  logic = ExactlyOne [Var Even, Var Odd]

instance HasLogicalModel Pairity Int where
  satisfiesProperty Even = even
  satisfiesProperty Odd = odd

instance HasPermutationGenerator Pairity Int where
  generators =
    [ Morphism
        { name = "switch"
        , match = Yes
        , contract =
            branches
              [ has Odd >> remove Odd >> add Even
              , has Even >> remove Even >> add Odd
              ]
        , morphism = switchPairity
        }
    ]

switchPairity :: Int -> Gen Int
switchPairity n =
  if
      | n == maxBound -> pure $ n - 1
      | n == minBound -> pure $ n + 1
      | otherwise -> element [n + 1, n -1]

instance HasParameterisedGenerator Pairity Int where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen Int
baseGen = int (linear minBound maxBound)

intPairityGenSelfTests :: TestTree
intPairityGenSelfTests =
  testGroup "intPairityGenSelfTests" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: Morphism Pairity Int) -> True)
        baseGen
