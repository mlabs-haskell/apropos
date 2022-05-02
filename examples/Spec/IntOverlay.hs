module Spec.IntOverlay (
  intSmplPermutationGenTests,
  IntSmpl (..),
) where

import Apropos

-- TODO this should probably be rexported eventually
import Apropos.Overlay
import Spec.IntPermutationGen (IntProp (IsNegative))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data IntSmpl = NonNegative
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel IntSmpl where
  logic = Yes

instance Overlay IntSmpl IntProp where
  overlays NonNegative = Not $ Var IsNegative

instance HasLogicalModel IntSmpl Int where
  satisfiesProperty = deduceFromOverlay

instance HasPermutationGenerator IntSmpl Int where
  sources = overlaySources

intSmplPermutationGenTests :: TestTree
intSmplPermutationGenTests =
  testGroup "intSmplPermutationGenTests" $
    pure $
      fromGroup $
        permutationGeneratorSelfTest @IntSmpl
