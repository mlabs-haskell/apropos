module Apropos (
  -- Apropos.Formula
  Formula (..),
  -- Apropos.Gen
  Gen,
  label,
  failWithFootnote,
  bool,
  int,
  list,
  shuffle,
  element,
  choice,
  genFilter,
  retry,
  -- Apropos.Gen.Range
  Range,
  linear,
  linearFrom,
  singleton,
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

import Apropos.Gen
import Apropos.Generator
import Apropos.Logic
import Apropos.Pure
import GHC.Generics (Generic)
