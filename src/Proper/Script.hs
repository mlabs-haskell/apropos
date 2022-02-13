{-# LANGUAGE TypeFamilies #-}

module Proper.Script (
  Proper (..),
  Proposition,
  Formula (..),
) where

import Data.Functor.Identity (Identity)
import Data.List (notElem)
import Data.Map.Lazy qualified as M
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Hedgehog (
  GenBase,
  Group (..),
  MonadGen,
  MonadTest,
  failure,
  footnote,
  footnoteShow,
  forAllWith,
  property,
  success,
--  (===),
 )
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Plutarch.Prelude
import Plutus.V1.Ledger.Api (ExCPU (..), ExMemory (..))
import Plutus.V1.Ledger.Scripts (Script, ScriptError (..), evaluateScript)
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (..))
import SAT.MiniSat (
  Formula (..),
  satisfiable,
  solve_all,
 )
import Text.PrettyPrint (
  Doc,
  Style (lineLength),
  colon,
  hang,
  int,
  renderStyle,
  style,
  text,
  vcat,
  ($+$),
  (<+>),
 )
import Text.Show.Pretty (ppDoc)
import Prelude (
  Bool (..),
  Bounded (..),
  Either (..),
  Enum(..),
  Eq,
  Int,
  Maybe (..),
  Ord,
  Show (..),
  String,
  all,
  any,
  id,
  filter,
  fmap,
  fst,
  snd,
  elem,
  zip,
  not,
  pure,
  length,
  ($),
  (&&),
  (.),
  (<$>),
  (<=),
  (>=),
  (<>),
  (==),
  (>>),
  (>),
  (<),
  (+),
  (||),
 )
import Control.Monad (mapM, msum,MonadPlus)

--------------------------------------------------------------------------------
-- Propositional logic is used to define two aspects of a model.
-- The expected outcome of a test and the sets of properties which are valid in
-- conjunction.
--------------------------------------------------------------------------------

type Proposition (a :: Type) = (Enum a, Eq a, Ord a, Bounded a, Show a)


satisfiesFormula :: forall p . Proposition p => Formula p -> Set p -> Bool
satisfiesFormula f s = satisfiable $ f :&&: All (Var <$> set) :&&: None (Var <$> unset)
  where
    set :: [p]
    set = Set.toList s
    unset :: [p]
    unset = filter (`notElem` s) ([minBound .. maxBound] :: [p])



-- Proper is a type family over a Model and its Properties
-- It encapsulates the model checking pattern shown in this diagram
--
-- Formula (Property model)        Set (Property model)
--            \                    /       ^
--             \                  /         \
--              \ genP         = /           \ satisfies
--               \              /      A      \
--                \            /               \
--                 v          /      genM       \
--          Set (Property model) -------------> Model model
--                 |                                    \
--                 |                                     \
--         expect  |                  B                   \ translate
--                 |                                       \
--                 v          =                 eval        v
--               Result ------------- Result <----------- Script
--
-- 'A' checks consistency between the model specification and its generator.
-- 'B' (which can be written after 'A' is complete) tests the compiled script.

class Proper model where

  -- a model encodes the data relevant to a specification
  -- this can be an arbitrary type
  data Model model :: Type

  -- properties are things that may be true of a model
  -- properties are Propositions
  data Property model :: Type

  -- modelTransformations are random functions from models that match a formula
  -- to models that satisfy a set of properties
  -- Transformations are Propositions
  data Transformation model :: Type

  -- check whether a property is satisfied
  satisfiesProperty :: Model model -> Property model -> Bool

  -- here we define the random functions named by the Transformations
  modelTransformation :: MonadGen m => Transformation model -> Model model -> m (Model model)

  --TODO return check as poperty test
  modelTransformationWithCheck :: (Show (Transformation model), Show (Model model), MonadGen m, Proposition (Property model), MonadTest t)
                               => Transformation model -> Model model -> m (t (), Model model)
  modelTransformationWithCheck t om = do
    let (r,i) = propertyTransformation t
    nm <- modelTransformation t om
    let ps = properties nm
    let prop = if (not (any (`elem` r) ps)) && (all (`elem` ps) i)
                  then pure ()
                  else genFailure om nm
    pure (prop,nm)
    where
      genFailure om' nm' =
        failWithFootnote $ renderStyle ourStyle $
           "Generator Transformation Invariant Failure."
              $+$ hang "Transformation:" 4 (ppDoc t)
              $+$ hang "Invariant:" 4 (ppDoc (propertyTransformation t))
              $+$ hang "FromModel:" 4 (ppDoc om')
              $+$ hang "FromProperties:" 4 (ppDoc (properties om'))
              $+$ hang "ToModel:" 4 (ppDoc nm')
              $+$ hang "ToProperties:" 4 (ppDoc (properties nm'))



  -- these are contracts that the model transformations must obey
  -- this is a tuple of (properties that will not hold after transformation, propties that will hold after transformation)
  -- these are used to find paths and are tested for correctness during path execution
  -- note that these are incomplete descriptions - the transformation may sometimes set or unset other properties
  -- for example by enforcing one property we may sometimes as a result toggle on another
  -- if genTransformation hits the search_depth without finding a satisfying model we revert to rejection sampling
  propertyTransformation :: Transformation model -> (Set (Property model), Set (Property model))

  --TODO check before search
  genTransformation :: Proposition (Transformation model)
                    => Proposition (Property model)
                    => Show (Model model)
                    => MonadGen m
                    => MonadPlus m
                    => MonadTest t
                    => Int
                    -> Model model
                    -> Set (Property model)
                    -> m (Maybe (t (), Model model))
  genTransformation depth_limit from to = go 0 (pure (), from)
    where
      applyPropertyTransformation t ps = let (r,i) = propertyTransformation t in i `Set.union` (ps `Set.difference` r)
      numConflicts a b = length $ filter id [ ((i `elem` a) && (not (i `elem` b))) || ((not (i `elem` a)) && (i `elem` b))
                                            | i <- [minBound..maxBound]
                                            ]
      go depth searchState = do
        let currentProperties = properties $ snd searchState
            decreasesConflicts t = (numConflicts (applyPropertyTransformation t currentProperties) to)
                                 < (numConflicts currentProperties to)
            conflictReducingPaths = filter decreasesConflicts [minBound..maxBound]
        crpShuffled <- Gen.shuffle conflictReducingPaths
        newModels <- mapM (\t -> modelTransformationWithCheck t (snd searchState)) crpShuffled
        let newModels' = (\(t,mo) -> (fst searchState >> t, mo)) <$> newModels
        case filter (\c -> properties (snd c) == to) newModels' of
          a:_ -> pure $ Just a
          [] -> if depth + 1 > depth_limit
                   then pure Nothing
                   else msum $ go (depth + 1) <$> newModels

  -- The base generator must be provided. It is parameterised by a set of properties.
  -- Ideally you write a generator that satisfies the contract provided by the set.
  -- Alternatively this check can be relaxed and transformations can be applied to the imperfect result.
  -- TODO transformations should form a STRICT contract and always be applied
  --   we check the transformations do what they say AND the result of genBaseModel >>= applyTransforms satisfies the set
  genBaseModel :: MonadGen m => Set (Property model) -> m (Model model)

  -- generates a model that satisfies a set of properties
  genModel :: Proposition (Transformation model)
           => Proposition (Property model)
           => Show (Model model)
           => MonadGen m
           => MonadPlus m
           => MonadTest t
           => Int
           -> Set (Property model)
           -> m (Model model, t (), (Set (Property model), Text))
  genModel search_depth targetProperties = do
    baseModel <- genBaseModel targetProperties
    if properties baseModel == targetProperties
       then pure (baseModel, pure (), (properties baseModel, "Direct Hit"))
       else do
         mModel <- genTransformation search_depth baseModel targetProperties
         case mModel of
           Just model' -> pure (snd model', fst model', (properties baseModel, "Transformation"))
           _ -> (,pure (), (properties baseModel, "Rejection Sampling")) <$> Gen.filterT (\candidateModel -> targetProperties == properties candidateModel) (genBaseModel targetProperties)

  -- propositional logic over model properties defines sets of properties valid in conjunction
  logic :: Formula (Property model)
  logic = Yes

  -- given a set of properties we expect a script to pass or fail
  expect :: Formula (Property model)
  expect = Yes

  enumerateScenariosWhere :: Proposition (Property model) => Formula (Property model) -> [Set (Property model)]
  enumerateScenariosWhere condition = enumerateSolutions $ logic :&&: condition :&&: allPresentInFormula
    where
      allPresentInFormula :: Formula (Property model)
      allPresentInFormula = All (mention <$> ([minBound .. maxBound] :: [Property model]))
      mention :: Property model -> Formula (Property model)
      mention p = Var p :||: Not (Var p)
      fromSolution :: Proposition p => M.Map p Bool -> Set p
      fromSolution m = Set.fromList $ filter isInSet [minBound .. maxBound]
        where
          isInSet k = Just True == M.lookup k m
      enumerateSolutions :: Proposition p => Formula p -> [Set p]
      enumerateSolutions f = fromSolution <$> solve_all f

  genGivenFormula :: (Proposition (Property model), MonadGen m, GenBase m ~ Identity) => Formula (Property model) -> m (Set (Property model))
  genGivenFormula f =
    let g = Set.fromList <$> Gen.subsequence [minBound .. maxBound]
     in Gen.filter (satisfiesFormula f) g

  -- compute the properties of a model
  properties ::
    Proposition (Property model) =>
    Model model ->
    Set (Property model)
  properties x = Set.fromList $ filter (satisfiesProperty x) [minBound .. maxBound]

  -- generates a set of properties (gen)
  genProperties ::
    (Proposition (Property model), MonadGen m, GenBase m ~ Identity) =>
    model ->
    m (Set (Property model))
  genProperties _ = genGivenFormula logic

  script :: Model model -> Maybe Script
  script _ = Nothing

  modelMemoryBounds :: Model model -> (ExMemory, ExMemory)
  modelMemoryBounds _ = (ExMemory minBound, ExMemory maxBound)

  modelCPUBounds :: Model model -> (ExCPU, ExCPU)
  modelCPUBounds _ = (ExCPU minBound, ExCPU maxBound)

  -- Script compiled code test (eval)
  -----------------------------------
  --

  runScriptTest ::
    Show (Model model) =>
    Proposition (Property model) =>
    MonadTest t =>
    Model model ->
    t ()
  runScriptTest model = do
    case script model of
      Nothing -> footnote "script not defined" >> failure
      Just so ->
        case evaluateScript so of
          Left (EvaluationError logs err) -> deliverResult model (Left (logs, err))
          Right res -> deliverResult model (Right res)
          Left err -> footnoteShow err >> failure

  deliverResult ::
    Show (Model model) =>
    Proposition (Property model) =>
    MonadTest m =>
    Model model ->
    Either ([Text], String) (ExBudget, [Text]) ->
    m ()
  deliverResult model res =
    case (shouldPass, res) of
      (False, Left _) -> success
      (True, Right (cost, _)) -> successWithBudgetCheck cost
      (True, Left err) -> failWithFootnote $ unexpectedFailure err
      (False, Right (_, logs)) -> failWithFootnote $ unexpectedSuccess logs
    where
      shouldPass :: Bool
      shouldPass = satisfiesFormula expect $ properties model
      successWithBudgetCheck :: MonadTest m => ExBudget -> m ()
      successWithBudgetCheck cost@(ExBudget cpu mem) =
        if inInterval cpu (modelCPUBounds model) && inInterval mem (modelMemoryBounds model)
          then success
          else failWithFootnote $ budgetCheckFailure cost
            where inInterval :: Ord a => a -> (a,a) -> Bool
                  inInterval a (l,u) = a >= l && a <= u
      budgetCheckFailure :: ExBudget -> String
      budgetCheckFailure cost =
        renderStyle ourStyle $
          "Success! But at what cost?"
            $+$ hang "Lower Bound" 4 (ppDoc (ExBudget (fst (modelCPUBounds model)) (fst (modelMemoryBounds model))))
            $+$ hang "Actual Cost" 4 (ppDoc cost)
            $+$ hang "Upper Bound" 4 (ppDoc (ExBudget (snd (modelCPUBounds model)) (snd (modelMemoryBounds model))))
      unexpectedSuccess :: [Text] -> String
      unexpectedSuccess logs =
        renderStyle ourStyle $
          "Unexpected success" $+$ dumpState logs
      unexpectedFailure :: ([Text], String) -> String
      unexpectedFailure (logs, reason) =
        renderStyle ourStyle $
          text ("Unexpected failure(" <> reason <> ")") $+$ dumpState logs
      dumpState :: [Text] -> Doc
      dumpState logs =
        ""
          $+$ hang "Inputs" 4 dumpInputs
          $+$ hang "Logs" 4 (dumpLogs logs)
          $+$ hang "Expected " 4 (if shouldPass then "Pass" else "Fail")
          $+$ hang "Properties " 4 (ppDoc $ properties model)
      dumpInputs :: Doc
      dumpInputs =
        "Parameters"
          $+$ ppDoc model
      dumpLogs :: [Text] -> Doc
      dumpLogs logs = vcat . fmap go . zip [1 ..] $ logs
      go :: (Int, Text) -> Doc
      go (ix, line) = (int ix <> colon) <+> (text . show $ line)



  -- HedgeHog properties and property groups

  modelTestGivenProperties ::
    Proposition (Transformation model) =>
    Proposition (Property model) =>
    Show (Model model) =>
    Int ->
    Set (Property model) ->
    Hedgehog.Property
  modelTestGivenProperties search_depth properties' =
    property $ do
      (model,t,g) <- forAllWith (\(mo,_,g) -> show (mo,g)) $ genModel search_depth properties'
      t
      if properties model == properties'
         then pure ()
         else failWithFootnote $ renderStyle ourStyle $
                                    "Model Consistency Failure."
                                      $+$ hang "Generator method:" 4 (ppGen g)
                                      $+$ hang  "Model:" 4 (ppDoc model)
                                      $+$ hang  "Expected:" 4 (ppDoc properties')
                                      $+$ hang  "Observed:" 4 (ppDoc (properties model))
    where ppGen (props,g)  = hang "Props:" 4 (ppDoc props)
                          $+$ hang "Generator:" 4 (ppDoc g)

  plutusTestGivenProperties ::
    Proposition (Transformation model) =>
    Proposition (Property model) =>
    Show (Model model) =>
    Int ->
    Set (Property model) ->
    Hedgehog.Property
  plutusTestGivenProperties search_depth properties' =
    property $ do
      (model,t,_) <- forAllWith (\(mo,_,g) -> show (mo,g))$ genModel search_depth properties'
      t
      runScriptTest model

  testEnumeratedScenarios ::
    Proposition (Transformation model) =>
    Proposition (Property model) =>
    Show (Model model) =>
    Show model =>
    model ->
    String ->
    (Set (Property model) -> Hedgehog.Property) ->
    Formula (Property model) ->
    Group
  testEnumeratedScenarios _ groupname test cond =
    Group (fromString groupname) $
      [ (fromString $ show $ Set.toList p, test p)
      | p <- enumerateScenariosWhere cond
      ]

failWithFootnote :: MonadTest m => String -> m a
failWithFootnote s = footnote s >> failure

ourStyle :: Style
ourStyle = style {lineLength = 80}

