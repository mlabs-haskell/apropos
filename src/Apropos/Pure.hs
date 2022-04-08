module Apropos.Pure (HasPureRunner (..)) where

import Apropos.Gen hiding ((===))
import Apropos.Gen.Enumerate
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.LogicalModel
import Apropos.Type
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Hedgehog (Group (..), Property, TestLimit, property, withTests, (===))

{- | Typeclass permitting model/property pairings to be run.

     Any value for which `script` returns `True` is expected to
     satisfy the `Formula` defined by `expect`.

     Simultaneously, any value for which the `expect` `Formula`
     is /unsatisfied/, should return `False`, when run by `script`.
-}
class
  (HasLogicalModel p m, HasParameterisedGenerator p m) =>
  HasPureRunner p m
  where
  -- | `expect` defines the expected behaviour of `script`. This
  --   relation must hold in both the positive and negative
  --   contexts. That is to say:
  --
  --   - When the `expect` expression /is/ satisfied by the
  --     properties of a value `m` then `script` must evaluate to
  --     `True`.
  --
  --   - When the `expect` expression /is not/ satisfied by the
  --     properties of a value `m` then `script` must evaluate to
  --     `False`.
  expect :: m :+ p -> Formula p

  -- | If a value `m` satisfies `expect`, then `script` should
  --   return `True`. Otherwise, it should return `False`.
  script :: m :+ p -> (m -> Bool)

  runPureTest :: m :+ p -> Set p -> Property
  runPureTest apropos s = property $ do
    (m :: m) <- handleRootRetries numRetries $ gen $ parameterisedGenerator s
    satisfiesFormula (expect apropos) s === script apropos m
    where
      numRetries :: Int
      numRetries = rootRetryLimit (Apropos :: m :+ p)

  runPureTestsWhere :: m :+ p -> String -> Formula p -> Group
  runPureTestsWhere pm name condition =
    Group (fromString name) $
      [ ( fromString $ show $ Set.toList scenario
        , runPureTest pm scenario
        )
      | scenario <- enumerateScenariosWhere condition
      ]

  enumeratePureTest :: m :+ p -> Set p -> Property
  enumeratePureTest apropos s = withTests (1 :: TestLimit) $
    property $ do
      let (ms :: [m]) = enumerate $ parameterisedGenerator s
          run m = satisfiesFormula (expect apropos) s === script apropos m
      sequence_ (run <$> ms)

  enumeratePureTestsWhere :: m :+ p -> String -> Formula p -> Group
  enumeratePureTestsWhere pm name condition =
    Group (fromString name) $
      [ ( fromString $ show $ Set.toList scenario
        , enumeratePureTest pm scenario
        )
      | scenario <- enumerateScenariosWhere condition
      ]
