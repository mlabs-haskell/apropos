module Apropos.HasParameterisedGenerator (
  HasParameterisedGenerator (..),
  runGeneratorTest,
  runGeneratorTestsWhere,
  enumerateGeneratorTest,
  enumerateGeneratorTestsWhere,
  genSatisfying,
) where

import Apropos.Gen hiding ((===))
import Apropos.Gen.Enumerate
import Apropos.HasLogicalModel
import Apropos.LogicalModel
import Apropos.Type
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Hedgehog (Group (..), Property, TestLimit, property, withTests, (===))

class (HasLogicalModel p m, Show m) => HasParameterisedGenerator p m where
  parameterisedGenerator :: Set p -> Gen m
  rootRetryLimit :: m :+ p -> Int
  rootRetryLimit _ = 100

--TODO caching calls to the solver in genSatisfying would probably be worth it
runGeneratorTest ::
  forall p m.
  HasParameterisedGenerator p m =>
  m :+ p ->
  Set p ->
  Property
runGeneratorTest _ s = property $ do
  (m :: m) <- handleRootRetries numRetries $ gen $ parameterisedGenerator s
  properties m === s
  where
    numRetries :: Int
    numRetries = rootRetryLimit (Apropos :: Apropos (m, p))

runGeneratorTestsWhere ::
  HasParameterisedGenerator p m =>
  m :+ p ->
  String ->
  Formula p ->
  Group
runGeneratorTestsWhere proxy name condition =
  Group (fromString name) $
    [ (fromString $ show $ Set.toList scenario, runGeneratorTest proxy scenario)
    | scenario <- enumerateScenariosWhere condition
    ]

enumerateGeneratorTest ::
  forall p m.
  HasParameterisedGenerator p m =>
  m :+ p ->
  Set p ->
  Property
enumerateGeneratorTest _ s = withTests (1 :: TestLimit) $
  property $ do
    let (ms :: [m]) = enumerate $ parameterisedGenerator s
        run m = properties m === s
    sequence_ (run <$> ms)

enumerateGeneratorTestsWhere ::
  HasParameterisedGenerator p m =>
  m :+ p ->
  String ->
  Formula p ->
  Group
enumerateGeneratorTestsWhere proxy name condition =
  Group (fromString name) $
    [ (fromString $ show $ Set.toList scenario, enumerateGeneratorTest proxy scenario)
    | scenario <- enumerateScenariosWhere condition
    ]

genSatisfying :: HasParameterisedGenerator p m => Formula p -> Gen m
genSatisfying f = do
  label $ fromString $ show f
  s <- element (enumerateScenariosWhere f)
  parameterisedGenerator s
