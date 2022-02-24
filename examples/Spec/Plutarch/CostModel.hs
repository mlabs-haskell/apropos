{-# LANGUAGE RankNTypes #-}

module Spec.Plutarch.CostModel (
  addCostPropGenTests,
  addCostModelPlutarchTests,
) where

import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.LogicalModel
import Apropos.Script

import qualified Data.Set as Set

import Plutus.V1.Ledger.Api (ExCPU (..), ExMemory (..))
import Plutus.V1.Ledger.Scripts (Script)

import Plutarch (compile)
import Plutarch.Prelude

import Data.Proxy (Proxy (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

numCostModels :: Integer
numCostModels = 10

peano :: Integer -> Term s PInteger
peano 0 = fromInteger 0
peano i = papp (plam (\p -> p + (fromInteger 1))) (peano (i -1))

addCost :: Integer -> Script
addCost i = compile $ peano i

data CostModelProp = ThisManyAdditions Integer
  deriving stock (Eq, Ord, Show)

instance Enumerable CostModelProp where
  enumerated = [ThisManyAdditions i | i <- [0 .. numCostModels]]

instance LogicalModel CostModelProp where
  logic =
    (ExactlyOne $ Var <$> enumerated)
      :&&: (Some $ Var <$> enumerated)

instance HasLogicalModel CostModelProp Integer where
  satisfiesProperty (ThisManyAdditions i) s = i == s

-- there is no randomness here
-- HasParameterisedEnumerator would make more sense for this.
instance HasParameterisedGenerator CostModelProp Integer where
  parameterisedGenerator s =
    case Set.toList s of
      [ThisManyAdditions i] -> pure i
      _ -> error "the impossible happened"

addCostPropGenTests :: TestTree
addCostPropGenTests =
  testGroup "Spec.Plutarch.CostModel" $
    fromGroup
      <$> [ runGeneratorTestsWhere
              (Proxy :: Proxy Integer)
              "(+) Cost Model Script Generator"
              (Yes :: Formula CostModelProp)
          ]

instance HasScriptRunner CostModelProp Integer where
  script _ i = addCost i
  expect _ _ = Yes :: Formula CostModelProp

  -- This is the cool bit. We can model the cost exactly. Neato.
  -- If we build a higherarchichal model we can compose these.
  modelMemoryBounds _ i =
    let cost = fromIntegral $ 200 + i * 702
     in (ExMemory cost, ExMemory cost)
  modelCPUBounds _ i =
    let cost = fromIntegral $ 29873 + i * 405620
     in (ExCPU cost, ExCPU cost)

addCostModelPlutarchTests :: TestTree
addCostModelPlutarchTests =
  testGroup "Plutarch.AdditionCostModel" $
    fromGroup
      <$> [ runScriptTestsWhere
              (Proxy :: Proxy Integer)
              (Proxy :: Proxy CostModelProp)
              "AdditionCostModel"
              Yes
          ]
