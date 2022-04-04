module Spec.PairityPair (
  PairityPair (..),
  pairityPairGenSelfTests,
) where

import Apropos
import Control.Lens.Tuple (_1, _2)
import Data.Bits (xor)
import Data.Functor ((<&>))
import GHC.Generics
import Spec.IntPairity
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data PairityPair
  = L Pairity
  | R Pairity
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable)

instance LogicalModel PairityPair where
  logic =
    ExactlyOne [Var $ L Odd, Var $ R Odd]
      :&&: abstractionLogic @(Int, Int)

instance HasLogicalModel PairityPair (Int, Int) where
  satisfiesProperty (L lp) (lm, _) = satisfiesProperty lp lm
  satisfiesProperty (R rp) (_, rm) = satisfiesProperty rp rm

instance HasAbstractions PairityPair (Int, Int) where
  abstractions =
    [ WrapAbs $
        ProductAbstraction
          { abstractionName = "left"
          , propertyAbstraction = abstractsProperties L
          , productModelAbstraction = _1
          }
    , WrapAbs $
        ProductAbstraction
          { abstractionName = "right"
          , propertyAbstraction = abstractsProperties R
          , productModelAbstraction = _2
          }
    ]

instance HasPermutationGenerator PairityPair (Int, Int) where
  generators = parallelAbstractionMorphisms

instance HasParameterisedGenerator PairityPair (Int, Int) where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen (Int, Int)
baseGen = do
  l <- int (linear minBound maxBound)
  r <- int (linear minBound maxBound)
  if odd l `xor` odd r
    then pure (l, r)
    else do
      choice
        [ (l,) <$> switchPairity r
        , switchPairity l <&> (,r)
        ]

pairityPairGenSelfTests :: TestTree
pairityPairGenSelfTests =
  testGroup "pairityPairGenSelfTests" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: Morphism PairityPair (Int, Int)) -> True)
        baseGen
