module AproposRoot (
  Description(..),
  v,
  allVariables,
  Formula (..),
  selfTest,
  selfTestWhere,
  satisfies,
  AproposTest (..),
  runTests,
  runTestsWhere,
  -- Usefull Reexports
  Generic,
  SOPGeneric,
  HasDatatypeInfo,
) where

import Apropos.Formula
import Apropos.Generator
import Apropos.Runner
import Apropos.Description