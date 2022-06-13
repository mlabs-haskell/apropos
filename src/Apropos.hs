module Apropos (
  -- Apropos.Logic
  Formula (..),
  Strategy (..),
  satisfiesExpression,
  -- Apropos.LogicalModel.Enumerable
  Enumerable (..),
  -- Apropos.LogicalModel.Formula
  -- Apropos.HasLogicalModel
  HasLogicalModel (..),
  Prop (Prop, unProp),
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
  Hashable,
  Generic,
) where

import Apropos.Gen
import Apropos.HasParameterisedGenerator
import Apropos.Logic
import Apropos.LogicalModel
import Apropos.LogicalModel.HasLogicalModel
import Apropos.Pure
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
