module Apropos (
  -- Apropos.Logic
  Formula (..),
  Strategy (..),
  satisfiesExpression,
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
  -- Apropos.HasParameterisedGenerator
  HasParameterisedGenerator (..),
  runGeneratorTest,
  runGeneratorTestsWhere,
  enumerateGeneratorTest,
  enumerateGeneratorTestsWhere,
  genSatisfying,
  sampleGenTest,
  -- Apropos.Pure
  PureRunner (..),
  runPureTest,
  runPureTestsWhere,
  enumeratePureTest,
  enumeratePureTestsWhere,
  -- Usefull Reexports
  Generic,
) where

import Apropos.Gen
import Apropos.HasParameterisedGenerator
import Apropos.Logic
import Apropos.Pure
import GHC.Generics (Generic)
