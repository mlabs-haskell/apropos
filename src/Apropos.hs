module Apropos (
  Description (describe, refineDescription, genDescribed),
  allAttributes,
  Formula (Yes, No, Not, (:&&:), (:||:), (:++:), (:->:), (:<->:), All, Some, None, ExactlyOne, AtMostOne),
  attr,
  selfTest,
  selfTestWhere,
  runTests,
  runTestsWhere,
  satisfies,
  AproposTest (AproposTest, expect, aproposTest),
  DeepHasDatatypeInfo,
  -- Usefull Reexports
  Generic,
  SOPGeneric,
  HasDatatypeInfo,
) where

import Apropos.Description
import Apropos.Formula
import Apropos.Generator
import Apropos.Runner
