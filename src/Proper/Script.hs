{-# LANGUAGE TypeFamilies #-}

module Proper.Script (
  Proper (..),
  Proposition,
  Formula (..),
) where
import Control.Monad.Reader (runReaderT, ReaderT)
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
  forAll,
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
  reverse,
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
  (||),
 )
import Control.Monad (join,foldM)

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

  -- transformations are random functions from models to models
  data Transformation model :: Type

  -- check whether a property is satisfied
  satisfiesProperty :: Model model -> Property model -> Bool

  -- propositional logic over model properties defines sets of properties valid in conjunction
  logic :: Formula (Property model)
  logic = Yes

  -- given a set of properties we expect a script to pass or fail
  expect :: Formula (Property model)
  expect = Yes

  -- here we define the random functions named by the Transformations
  modelTransformation :: MonadGen m => Transformation model -> Model model -> m (Model model)

  propertyTransformation :: Transformation model -> (Formula (Property model), Set (Property model) -> Set (Property model))

  genBaseModel :: MonadGen m => ReaderT (Set (Property model)) m (Model model)

  -- generates a model that satisfies a set of properties
  genModel :: Proposition (Transformation model)
           => Proposition (Property model)
           => Show (Model model)
           => MonadGen m
           => Set (Property model)
           -> m (Model model,[Transformation model])
  genModel targetProperties = do
    baseModel <- runReaderT genBaseModel targetProperties
    let transforms = enumeratePaths (properties baseModel) targetProperties
    case transforms of
      [] -> pure (baseModel,[])
      _ -> do
        transform <- Gen.element transforms
        pure (baseModel,transform)

  enumeratePaths :: Proposition (Transformation model)
                     => Proposition (Property model)
                     => Show (Model model)
                     => Set (Property model)
                     -> Set (Property model)
                     -> [[Transformation model]]
  enumeratePaths from to =
    if from == to
       then []
       else go [] from
    where
      allTransformations :: [Transformation model]
      allTransformations = [minBound..maxBound]
      applicableTransformations s = filter (\t -> satisfiesFormula (fst (propertyTransformation t)) s) allTransformations
      numConflicts :: (Bounded x, Eq x, Enum x) => Set x -> Set x -> Int
      numConflicts a b = length $ filter id [ ((i `elem` a) && (not (i `elem` b))) || ((not (i `elem` a)) && (i `elem` b))
                                            | i <- [minBound..maxBound]
                                            ]
      reducesConflicts :: Set (Property model) -> Transformation model -> Bool
      reducesConflicts s t = numConflicts s to > numConflicts ((snd (propertyTransformation t)) s) to
      go :: [Transformation model] -> Set (Property model) -> [[Transformation model]]
      go breadcrumbs s =
        let candidatePaths :: [Transformation model]
            candidatePaths = applicableTransformations s
            conflictReducingPaths :: [Transformation model]
            conflictReducingPaths = filter (reducesConflicts s) candidatePaths
            thatDon'tDoubleBack :: [Transformation model]
            thatDon'tDoubleBack = filter (\t -> not (t `elem` breadcrumbs)) conflictReducingPaths
            thatReachDestination :: [Transformation model]
            thatReachDestination = filter (\t -> (snd (propertyTransformation t)) s == to) thatDon'tDoubleBack
            incomplete :: [Transformation model]
            incomplete = filter (\t -> not (t `elem` thatReachDestination)) thatDon'tDoubleBack
            continuePath :: Transformation model -> [[Transformation model]]
            continuePath t = go (t:breadcrumbs) ((snd (propertyTransformation t)) s)
        in ((\t -> reverse (t:breadcrumbs)) <$> thatReachDestination) <> (join (continuePath <$> incomplete))


  modelTransformationWithCheck :: (Show (Transformation model), Show (Model model), Proposition (Property model), MonadTest t, MonadGen m)
                               => (t (), Model model) -> Transformation model -> m (t (), Model model)
  modelTransformationWithCheck (check, om) t = do
    let (_,i) = propertyTransformation t
    nm <- modelTransformation t om
    if properties nm == i (properties om)
      then pure (check, nm)
      else pure (check >> genFailure om nm, nm)
    where
      genFailure om' nm' =
        failWithFootnote $ renderStyle ourStyle $
           "Generator Transformation Invariant Failure."
              $+$ hang "Transformation:" 4 (ppDoc t)
              $+$ hang "FromModel:" 4 (ppDoc om')
              $+$ hang "FromProperties:" 4 (ppDoc (properties om'))
              $+$ hang "ToModel:" 4 (ppDoc nm')
              $+$ hang "ToProperties:" 4 (ppDoc (properties nm'))

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
    Set (Property model) ->
    Hedgehog.Property
  modelTestGivenProperties properties' =
    property $ do
      (model',transforms) <- forAll $ genModel properties'
      (check, model) <- forAllWith (\(_,m) -> show m) $ foldM modelTransformationWithCheck (pure (), model') transforms
      check
      if properties model' == properties'
         then pure ()
         else failWithFootnote $ renderStyle ourStyle $
                                    "Model Consistency Failure."
                                      $+$ hang  "Model:" 4 (ppDoc model)
                                      $+$ hang  "Transformation:" 4 (ppDoc transforms)
                                      $+$ hang  "Expected:" 4 (ppDoc properties')
                                      $+$ hang  "Observed:" 4 (ppDoc (properties model))

  plutusTestGivenProperties ::
    Proposition (Transformation model) =>
    Proposition (Property model) =>
    Show (Model model) =>
    Set (Property model) ->
    Hedgehog.Property
  plutusTestGivenProperties properties' =
    property $ do
      (model',transforms) <- forAll $ genModel properties'
      (check,model) <- forAllWith (\(_,m) -> show m) $ foldM modelTransformationWithCheck (pure (), model') transforms
      check
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

