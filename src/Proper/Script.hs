{-# LANGUAGE TypeFamilies #-}

module Proper.Script (
  Proper (..),
  Proposition,
  Formula (..),
  Toggle (..),
  on,
  off,
) where

import Data.Functor.Identity (Identity)
import Data.List (notElem)
import Data.Map.Lazy qualified as M
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map (Map)
import Data.Map qualified as Map
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
  (===),
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
  MonadFail,
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
  filter,
  length,
  mod,
  foldr,
  fmap,
  fst,
  snd,
  zip,
  pure,
  fail,
  ($),
  (&&),
  (.),
  (<$>),
  (<=),
  (>=),
  (<>),
  (==),
  (/=),
  (>>),
  (+),
  (-),
  (>),
  error,
 )
import Control.Monad (join)
import Data.Graph (Graph, buildG, scc)

--------------------------------------------------------------------------------
-- Propositional logic is used to define two aspects of a model.
-- The expected outcome of a test and the sets of properties which are valid in
-- conjunction.
--------------------------------------------------------------------------------

type Proposition (a :: Type) = (Enum a, Eq a, Ord a, Bounded a, Show a)

data Toggle a = On a | Off a
  deriving stock (Eq,Ord,Show)

instance (Enum a, Bounded a) => Enum (Toggle a) where
  toEnum i =
    let sizeA = length ([minBound..maxBound] :: [a])
     in if i >= sizeA
          then Off (toEnum (i `mod` sizeA))
          else On (toEnum i)
  fromEnum (On a) = fromEnum a
  fromEnum (Off a) =
    let sizeA = length ([minBound..maxBound] :: [a])
     in sizeA + fromEnum a

instance Bounded a => Bounded (Toggle a) where
  minBound = On minBound
  maxBound = Off maxBound

on :: p -> Formula (Toggle p)
on = Var . On

off :: p -> Formula (Toggle p)
off = Var . Off

toggle :: Ord a => Toggle a -> Set a -> Set a
toggle (On a) = Set.insert a
toggle (Off a) = Set.delete a

toggleSet :: Ord a => Set (Toggle a) -> Set a -> Set a
toggleSet toggles s = foldr toggle s toggles

conflictSet :: Ord a => Set a -> Set a -> Set (Toggle a)
conflictSet a b = (Set.map On (b `Set.difference` a))
        `Set.union` (Set.map Off (a `Set.difference` b))

satisfiesFormula :: forall p . Proposition p => Formula p -> Set p -> Bool
satisfiesFormula f s = satisfiable $ f :&&: All (Var <$> set) :&&: None (Var <$> unset)
  where
    set :: [p]
    set = Set.toList s
    unset :: [p]
    unset = filter (`notElem` s) ([minBound .. maxBound] :: [p])


enumerateSolutions :: Proposition p => Formula p -> [Set p]
enumerateSolutions f = fromSolution <$> solve_all f
  where
    fromSolution :: Proposition p => M.Map p Bool -> Set p
    fromSolution m = Set.fromList $ filter isInSet [minBound .. maxBound]
      where
        isInSet k = Just True == M.lookup k m



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

  -- check whether a property is satisfied
  satisfiesProperty :: Model model -> Property model -> Bool

  -- propositional logic over model properties defines sets of properties valid in conjunction
  logic :: Formula (Property model)
  logic = Yes

  -- given a set of properties we expect a script to pass or fail
  expect :: Formula (Property model)
  expect = Yes

  transformation :: MonadGen m => Toggle (Property model) -> Model model -> m (Model model)
  transformation _ m = pure m

  transformationImplications :: Proposition (Property model) => Toggle (Property model) -> Formula (Toggle (Property model))
  transformationImplications _ = Yes

  transformationInvariant :: Proposition (Property model) => Toggle (Property model) -> Formula (Toggle (Property model))
  transformationInvariant t = Var t:&&: transformationImplications t



  transformationPossible :: Proposition (Property model) => Toggle (Property model) -> Formula (Property model)
  transformationPossible _ = No

  transformationPossible' :: Proposition (Property model) => Toggle (Property model) -> Formula (Property model)
  transformationPossible' t@(On o) = Not (Var o) :&&: transformationPossible t
  transformationPossible' t@(Off o) = Var o :&&: transformationPossible t


  buildTransformGraph :: Proposition (Property model)
                      => model
                      -> (Map (Set (Property model)) Int
                         ,Map Int (Set (Property model))
                         ,Graph
                         )
  buildTransformGraph _ =
    let scenarios = enumerateScenariosWhere logic
        scenmap = Map.fromList $ zip scenarios [0..]
        edgemap = Map.fromList
                $ zip [0..] (transfromationEdgesFrom scenmap scenarios <$> scenarios)
        intgraph = buildG (0,(length scenarios)-1) $ flattenTransformationEdges edgemap

      in (scenmap
         ,Map.fromList $ zip [0..] scenarios
         ,intgraph
         )
    where
      transfromationEdgesFrom :: Map (Set (Property model)) Int
                              -> [Set (Property model)]
                              -> Set (Property model)
                              -> [(Int, (Toggle (Property model)))]
      transfromationEdgesFrom scenmap scenarios scene =
        let lut l = case Map.lookup l scenmap of
                      Nothing -> error "this should never happen"
                      Just so -> so
            cases = [(lut scenario,t)
                    | scenario <- scenarios
                    , scene /= scenario
                    , t <- filter (\t' -> satisfiesFormula (transformationPossible' t') scene) [minBound..maxBound]
                    , let cs = (conflictSet scene scenario)
                    , satisfiesFormula (transformationInvariant t) cs
                    ]
          in cases

      flattenTransformationEdges :: Map Int [(Int, (Toggle (Property model)))]
                                 -> [(Int,Int)]
      flattenTransformationEdges m = join (toEdges <$> Map.toList m)
        where toEdges (f,es) = (f,) <$> (fst <$> es)


  genModel :: MonadGen m => m (Model model)

  traverseTransformGraph :: Proposition (Property model)
                         => Show (Model model)
                         => MonadGen m
                         => MonadFail m
                         => MonadTest t
                         => Model model
                         -> Set (Property model)
                         -> m (t (), Model model,[Toggle (Property model)])
  traverseTransformGraph baseModel targetProperties = go [] (pure ()) baseModel
    where
      go breadcrumbs check mo = do
        if length breadcrumbs > 100
           then fail "tried 100 transformations and gave up"
           else pure ()
        let cs = conflictSet (properties baseModel) targetProperties
        t <- Gen.element $ Set.toList cs
        (check',nm) <- transformationWithCheck breadcrumbs t mo
        if properties nm == targetProperties
           then pure (check >> check', nm, breadcrumbs <> [t])
           else go (breadcrumbs <> [t]) (check >> check') nm


  transformationWithCheck :: (Show (Toggle (Property model)), Show (Model model), Proposition (Property model), MonadTest t, MonadGen m)
                          => [Toggle (Property model)] -> Toggle (Property model) -> Model model -> m (t (), Model model)
  transformationWithCheck pathSoFar t om = do
    nm <- transformation t om
    let cs = conflictSet (properties om) (properties nm)
    if satisfiesFormula (transformationInvariant t) cs
      then if transformationIsSound cs
             then pure (pure () , nm)
             else pure (transformationLogicInconsistency nm, om)
      else pure (genFailure nm, nm)
    where
      transformationIsSound cs = satisfiesFormula logic (toggleSet cs (properties om))
      transformationLogicInconsistency nm =
        failWithFootnote $ renderStyle ourStyle $
           "Transformation Logic Inconsistency."
             $+$ logs nm
      genFailure nm =
        failWithFootnote $ renderStyle ourStyle $
           "Transformation Invariant Failure."
             $+$ logs nm
      logs nm =  hang "Path so far:" 4 (ppDoc pathSoFar)
              $+$ hang "Transformation:" 4 (ppDoc t)
              $+$ hang "FromModel:" 4 (ppDoc om)
              $+$ hang "FromProperties:" 4 (ppDoc (properties om))
              $+$ hang "ToModel:" 4 (ppDoc nm)
              $+$ hang "ToProperties:" 4 (ppDoc (properties nm))

  enumerateScenariosWhere :: Proposition (Property model) => Formula (Property model) -> [Set (Property model)]
  enumerateScenariosWhere condition = enumerateSolutions $ logic :&&: condition :&&: allPresentInFormula
    where
      allPresentInFormula :: Formula (Property model)
      allPresentInFormula = All (mention <$> ([minBound .. maxBound] :: [Property model]))
      mention :: Property model -> Formula (Property model)
      mention p = Var p :||: Not (Var p)

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

  testTransformations ::
    Proposition (Property model) =>
    Show (Model model) =>
    model ->
    String ->
    Hedgehog.Group
  testTransformations model groupname = Group (fromString groupname) [
      ("Transformation Graph is fully connected"
      , property $ do
          let (_,_,g) = buildTransformGraph model
          1 === length (scc g)
      )
    ]

  modelTestGivenProperties ::
    Proposition (Property model) =>
    Show (Model model) =>
    Set (Property model) ->
    Hedgehog.Property
  modelTestGivenProperties targetProperties =
    property $ do
      baseModel <- forAll genModel
      (check, model, path) <- forAllWith (\(_,m,_) -> show m) $ traverseTransformGraph baseModel targetProperties
      check
      if properties model == targetProperties -- redundant?
         then pure ()
         else failWithFootnote $ renderStyle ourStyle $
                                    "Model Consistency Failure."
                                      $+$ hang  "Model:" 4 (ppDoc model)
                                      $+$ hang  "Path:" 4 (ppDoc path)
                                      $+$ hang  "Expected:" 4 (ppDoc targetProperties)
                                      $+$ hang  "Observed:" 4 (ppDoc (properties model))

  plutusTestGivenProperties ::
    Proposition (Property model) =>
    Show (Model model) =>
    Set (Property model) ->
    Hedgehog.Property
  plutusTestGivenProperties targetProperties =
    property $ do
      baseModel <- forAll genModel
      (check,model,_) <- forAllWith (\(_,m,_) -> show m) $ traverseTransformGraph baseModel targetProperties
      check
      runScriptTest model

  testEnumeratedScenarios ::
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

