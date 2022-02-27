module Apropos (
  -- Apropos.Type
  Apropos(..),
  (:+),

  -- Apropos.LogicalModel
  LogicalModel(..),

  -- Apropos.LogicalModel.Enumerable
  Enumerable(..),
  gen_enumerable,

  -- Apropos.LogicalModel.Formula
  Formula(..),

  -- Apropos.HasLogicalModel
  HasLogicalModel(..),

  -- Apropos.Gen
  Gen,
  Gen',
  liftGen,
  source,
  sink,
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
  singleton,

  -- Apropos.HasParameterisedGenerator
  HasParameterisedGenerator (..),
  runGeneratorTest,
  runGeneratorTestsWhere,
  genSatisfying,

  -- Apropos.HasPermutationGenerator
  HasPermutationGenerator(..),
  PermutationEdge(..),
  Abstraction(..),
  abstract,
  abstractsProperties,
  (|:->),

  -- Apropos.HasPermutationGenerator.Contract
  Contract,
  runContract,
  readContractInput,
  readContractEdgeName,
  readContractOutput,
  writeContractOutput,
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
  output,
  contractError,
  terminal,

  -- Apropos.Pure
  HasPureRunner(..),

  )
  where
import Apropos.Type
import Apropos.LogicalModel
import Apropos.HasLogicalModel
import Apropos.Gen
import Apropos.HasParameterisedGenerator
import Apropos.HasPermutationGenerator
import Apropos.HasPermutationGenerator.Contract
import Apropos.Pure
