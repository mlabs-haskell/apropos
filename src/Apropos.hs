module Apropos (
  -- Apropos.Formula
  Formula (..),
  -- Apropos.Generator
  selfTest,
  selfTestWhere,
  genSatisfying,
  sampleGenTest,
  -- Apropos.Runner
  AproposTest(..),
  runTests,
  runTestsWhere,
  -- Usefull Reexports
  Generic,
) where

import Apropos.Generator
import Apropos.Logic
import Apropos.Runner
import GHC.Generics (Generic)
