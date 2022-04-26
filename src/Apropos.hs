module Apropos (
  -- Apropos.Type
  Apropos (..),
  (:+),
  -- Apropos.LogicalModel
  LogicalModel (..),
  -- Apropos.LogicalModel.Enumerable
  Enumerable (..),
  -- Apropos.LogicalModel.Formula
  Formula (..),
  -- Apropos.HasLogicalModel
  HasLogicalModel (..),
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
  -- Apropos.Gen.Traversal
  Traversal (..),
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
  -- Apropos.HasPermutationGenerator
  HasPermutationGenerator (..),
  Morphism (..),
  Abstraction (..),
  abstract,
  gotoSum,
  abstractsProperties,
  (&&&),
  (>>>),
  -- Apropos.HasAbstractions
  HasAbstractions (abstractions),
  AbstractionFor (WrapAbs),
  abstractionMorphisms,
  parallelAbstractionMorphisms,
  abstractionLogic,
  -- Apropos.HasPermutationGenerator.Contract
  Contract,
  runContract,
  branches,
  branchIf,
  has,
  hasn't,
  hasAll,
  hasNone,
  add,
  addIf,
  addAll,
  addAllIf,
  remove,
  removeIf,
  removeAll,
  removeAllIf,
  clear,
  terminal,
  matches,
  -- Apropos.Pure
  HasPureRunner (..),
) where

import Apropos.Gen
import Apropos.Gen.BacktrackingTraversal
import Apropos.HasAbstractions
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.HasPermutationGenerator
import Apropos.HasPermutationGenerator.Contract
import Apropos.LogicalModel
import Apropos.Pure
import Apropos.Type
