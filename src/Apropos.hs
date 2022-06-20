module Apropos (
  -- Apropos.Formula
  Formula (..),
  -- Apropos.Generator
  selfTest,
  selfTestWhere,
  genSatisfying,
  sampleGenTest,
  -- Apropos.Pure
  runPureTest,
  runPureTestsWhere,
  -- Usefull Reexports
  Generic,
) where

import Apropos.Generator
import Apropos.Logic
import Apropos.Pure
import GHC.Generics (Generic)
