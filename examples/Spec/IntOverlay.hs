module Spec.IntOverlay
  (intSmplPermutationGenTests
  ,IntSmpl(..)) where

import Apropos
-- TODO this should probably be rexported eventually
import Apropos.Overlay
import Spec.IntPermutationGen ( IntProp(IsNegative) )
import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.Hedgehog ( fromGroup )

data IntSmpl = NonNegative
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel IntSmpl where
  logic = Yes

instance Overlay IntSmpl IntProp where
  overlays NonNegative = Not $ Var IsNegative

-- TODO ideally you wouldn't need even this but I can't get it to work as a generic instance
deriving anyclass instance HasLogicalModel IntSmpl Int

instance HasPermutationGenerator IntSmpl Int where
  sources =
    [ Source
      {sourceName = "overlay"
      , covers = Yes
      , pgen = \ps -> genSatisfying (All [ (if p `elem` ps then id else Not) $ overlays p | p <- enumerated ])
      }
    , Source
      { sourceName = ""
      , covers = Var NonNegative
      , pgen = const $ pure 2
      }
    ]

intSmplPermutationGenTests :: TestTree
intSmplPermutationGenTests =
  testGroup "intSmplPermutationGenTests" $
    pure $
      fromGroup $
        permutationGeneratorSelfTest @IntSmpl
