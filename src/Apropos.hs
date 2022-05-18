module Apropos (
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
  sampleGenTest,
  -- Apropos.HasPermutationGenerator
  HasPermutationGenerator (..),
  Morphism (..),
  Source (..),
  (&&&),
  (>>>),
  -- Apropos.HasAbstractions
  HasAbstractions (..),
  ProductAbstraction (..),
  SumAbstraction (..),
  SourceAbstraction (..),
  ProductAbstractionFor (..),
  SumAbstractionFor (..),
  SourceAbstractionFor (..),
  PAbs (..),
  abstractionMorphisms,
  abstractionSources,
  parallelAbstractionMorphisms,
  abstractionLogic,
  abstractsProperties,
  -- Apropos.HasPermutationGenerator.Contract
  Contract,
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
  forget,
  deduce,
  swap,
  toggle,
  -- Apropos.Pure
  PureRunner (..),
  runPureTest,
  runPureTestsWhere,
  enumeratePureTest,
  enumeratePureTestsWhere,
  -- Usefull Reexports
  Hashable,
  Generic,
  -- Overlay
  Overlay (overlays),
  soundOverlay,
  deduceFromOverlay,
  overlaySources,
) where

import Apropos.Gen
import Apropos.Gen.BacktrackingTraversal
import Apropos.HasAbstractions
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.HasPermutationGenerator
import Apropos.HasPermutationGenerator.Contract
import Apropos.LogicalModel
import Apropos.Overlay
import Apropos.Pure
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
