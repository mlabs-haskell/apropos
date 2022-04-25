module Spec.IntOverlay (IntSmpl(..)) where

import Apropos
-- TODO this should probably be rexported eventually
import Apropos.Overlay
import Spec.IntPermutationGen

data IntSmpl = NonNegative
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel IntSmpl where
  logic = Yes

instance Overlay IntSmpl IntProp where
  overlays NonNegative = Not $ Var IsNegative

-- TODO ideally you wouldn't need even this but I can't get it to work as a generic instance
deriving anyclass instance HasLogicalModel IntSmpl Int
