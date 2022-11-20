{- |
Description: Hedgehog testing that sniffs out edge cases

Apropos allows you to describe what test data may trigger edge cases in your code, and automatically exhaustively test against them. see the [README](https://github.com/mlabs-haskell/apropos#readme) for a fuller explanation.
-}
module Apropos (
  Description (describe, refineDescription, genDescribed),

  -- * Formulas

  -- | Numerous Apropos features use 'Formula's to specify subsets of description types. The 'refineDescription' method is a 'Formula', and the 'satisfies' combinator allows using a 'Formula' to create a predicate for passing to the test runners.
  --
  --  A 'Formula' is an expression built of attributes, specified using the 'attr' combinator, and operators specified by the constructors of the 'Formula' type.
  attr,
  FieldSelector,
  Formula (
    Yes,
    No,
    Not,
    (:&&:),
    (:||:),
    (:++:),
    (:->:),
    (:<->:),
    All,
    Some,
    None,
    ExactlyOne,
    AtMostOne
  ),
  allAttributes,
  satisfies,

  -- * Test runners
  selfTest,
  selfTestWhere,
  runTests,
  runTestsWhere,
  Outcome (Pass, Fail),
  passIf,
  OptOutcome (Run, Ignore),

  -- * Utility type
  DeepGeneric,
) where

import Apropos.Description (
  DeepGeneric,
  Description (describe, genDescribed, refineDescription),
  FieldSelector,
  Formula (
    All,
    AtMostOne,
    ExactlyOne,
    No,
    None,
    Not,
    Some,
    Yes,
    (:&&:),
    (:++:),
    (:->:),
    (:<->:),
    (:||:)
  ),
  allAttributes,
  attr,
  satisfies,
 )
import Apropos.Generator (selfTest, selfTestWhere)
import Apropos.Runner (OptOutcome (Ignore, Run), Outcome (Fail, Pass), passIf, runTests, runTestsWhere)
