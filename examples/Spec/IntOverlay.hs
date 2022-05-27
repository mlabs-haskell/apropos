module Spec.IntOverlay (
  intSmplPermutationGenTests,
  IntSmpl (..),
) where

import Apropos
import Apropos.LogicalModel
import Spec.IntPermutationGen (IntProp (IsNegative))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup, testProperty)

data IntSmpl = NonNegative
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel IntSmpl where
  logic = Yes

instance Overlay (Prop IntSmpl) (Prop IntProp) Int Int where
  overlays (Prop NonNegative) = Not $ Var (Prop IsNegative)

instance HasLogicalModel IntSmpl Int where
  satisfiesProperty = deduceFromOverlay . Prop

instance HasPermutationGenerator (Prop IntSmpl) Int where
  sources = overlaySources

instance HasParameterisedGenerator (Prop IntSmpl) Int where
  parameterisedGenerator = buildGen @(Prop IntSmpl)

intSmplPermutationGenTests :: TestTree
intSmplPermutationGenTests =
  testGroup
    "intSmplPermutationGenTests"
    [ testProperty "overlay is sound" $ soundOverlay @(Prop IntSmpl)
    , fromGroup $ permutationGeneratorSelfTest @(Prop IntSmpl)
    ]
