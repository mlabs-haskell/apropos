module Apropos.LogicalModel (
  LogicalModel (..),
  Enumerable (..),
  module Apropos.LogicalModel.Enumerable,
) where

import Apropos.Logic
import Apropos.LogicalModel.Enumerable

class (Eq p, Ord p, Show p) => LogicalModel p where
  logic :: Formula p

