{-# LANGUAGE TypeFamilies #-}

module Spec.IntPermutationGen (
  intPermutationGenTests,
  intPermutationGenPureTests,
  intPermutationGenPlutarchTests,
  intPermutationGenSelfTests,
  IntProp (..),
) where

import Apropos.Gen
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.HasPermutationGenerator
import Apropos.HasPermutationGenerator.Contract
import Apropos.LogicalModel
import Apropos.Pure
import Apropos.Script
import Data.Proxy (Proxy (..))
import Plutarch (compile)
import Plutarch.Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data IntProp
  = IsNegative
  | IsPositive
  | IsZero
  | IsLarge
  | IsSmall
  | IsMaxBound
  | IsMinBound
  deriving stock (Eq, Ord, Enum, Show, Bounded)

instance Enumerable IntProp where
  enumerated = [minBound .. maxBound]

instance LogicalModel IntProp where
  logic =
    ExactlyOne [Var IsNegative, Var IsPositive, Var IsZero]
      :&&: ExactlyOne [Var IsLarge, Var IsSmall]
      :&&: (Var IsZero :->: Var IsSmall)
      :&&: (Var IsMaxBound :->: (Var IsLarge :&&: Var IsPositive))
      :&&: (Var IsMinBound :->: (Var IsLarge :&&: Var IsNegative))

instance HasLogicalModel IntProp Int where
  satisfiesProperty IsNegative i = i < 0
  satisfiesProperty IsPositive i = i > 0
  satisfiesProperty IsMaxBound i = i == maxBound
  satisfiesProperty IsMinBound i = i == minBound
  satisfiesProperty IsZero i = i == 0
  satisfiesProperty IsLarge i = i > 10 || i < -10
  satisfiesProperty IsSmall i = i <= 10 && i >= -10

instance HasPermutationGenerator IntProp Int where
  generators =
    [ PermutationEdge
        { name = "MakeZero"
        , match = Not $ Var IsZero
        , contract = clear >> addAll [IsZero, IsSmall]
        , permuteGen = pure 0
        }
    , PermutationEdge
        { name = "MakeMaxBound"
        , match = Not $ Var IsMaxBound
        , contract = clear >> addAll [IsMaxBound, IsLarge, IsPositive]
        , permuteGen = pure maxBound
        }
    , PermutationEdge
        { name = "MakeMinBound"
        , match = Not $ Var IsMinBound
        , contract = clear >> addAll [IsMinBound, IsLarge, IsNegative]
        , permuteGen = pure minBound
        }
    , PermutationEdge
        { name = "MakeLarge"
        , match = Not $ Var IsLarge
        , contract = clear >> addAll [IsLarge, IsPositive]
        , permuteGen = int (linear 11 (maxBound -1))
        }
    , PermutationEdge
        { name = "MakeSmall"
        , match = Not $ Var IsSmall
        , contract = clear >> addAll [IsSmall, IsPositive]
        , permuteGen = int (linear 1 10)
        }
    , PermutationEdge
        { name = "Negate"
        , match = Not $ Var IsZero
        , contract =
            branches
              [ has IsNegative >> remove IsNegative >> add IsPositive
              , has IsPositive >> remove IsPositive >> add IsNegative
              ]
        , permuteGen = do
            i <- source
            pure (- i)
        }
    ]

instance HasParameterisedGenerator IntProp Int where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen Int Int
baseGen = int (linear minBound maxBound)

intPermutationGenTests :: TestTree
intPermutationGenTests =
  testGroup "intPermutationGenTests" $
    fromGroup
      <$> [ runGeneratorTestsWhere (Proxy :: Proxy Int) "Int Generator" (Yes :: Formula IntProp)
          ]

instance HasPureRunner IntProp Int where
  expect _ = Var IsSmall :&&: Var IsNegative
  script _ i = i < 0 && i >= -10

intPermutationGenPureTests :: TestTree
intPermutationGenPureTests =
  testGroup "intPermutationGenPureTests" $
    fromGroup
      <$> [ runPureTestsWhere (Proxy :: Proxy Int) "AcceptsSmallNegativeInts" (Yes :: Formula IntProp)
          ]

instance HasScriptRunner IntProp Int where
  expect _ _ = Var IsSmall :&&: Var IsNegative
  script _ i =
    let ii = (fromIntegral i) :: Integer
     in compile (pif (((fromInteger ii) #< ((fromInteger 0) :: Term s PInteger)) #&& (((fromInteger (-10)) :: Term s PInteger) #<= (fromInteger ii))) (pcon PUnit) perror)

intPermutationGenPlutarchTests :: TestTree
intPermutationGenPlutarchTests =
  testGroup "intPermutationGenPlutarchTests" $
    fromGroup
      <$> [ runScriptTestsWhere (Proxy :: Proxy Int) (Proxy :: Proxy IntProp) "AcceptsSmallNegativeInts" Yes
          ]

intPermutationGenSelfTests :: TestTree
intPermutationGenSelfTests =
  testGroup "intPermutationGenSelfTests" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: PermutationEdge IntProp Int) -> True)
        baseGen
