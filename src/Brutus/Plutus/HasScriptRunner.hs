module Brutus.Plutus.HasScriptRunner (HasScriptRunner(..)) where
import Brutus.LogicalModel
import Brutus.HasLogicalModel
import Brutus.HasParameterisedGenerator

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Hedgehog (
  Property,
  MonadTest,
  Group(..),
  failure,
  footnote,
  footnoteShow,
  success,
  property,
 )
import Data.String (fromString)
import Plutus.V1.Ledger.Api (ExCPU (..), ExMemory (..))
import Plutus.V1.Ledger.Scripts (Script, ScriptError (..), evaluateScript)
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (..))
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
  Int,
  Ord,
  Show (..),
  String,
  fmap,
  fst,
  snd,
  zip,
  ($),
  (&&),
  (.),
  (<=),
  (>=),
  (<>),
  (>>),
 )
import Data.Proxy (Proxy(..))

class (HasLogicalModel p m, HasParameterisedGenerator p m) => HasScriptRunner p m where
  script :: Proxy p -> m -> Script
  expect :: Proxy m -> Proxy p -> Formula p

  modelMemoryBounds :: Proxy p -> m -> (ExMemory, ExMemory)
  modelMemoryBounds _ _ = (ExMemory minBound, ExMemory maxBound)

  modelCPUBounds :: Proxy p -> m -> (ExCPU, ExCPU)
  modelCPUBounds _ _ = (ExCPU minBound, ExCPU maxBound)

  runScriptTestsWhere :: (Proxy m) -> (Proxy p) -> String -> Formula p -> Group
  runScriptTestsWhere mprox _ name condition =
    Group (fromString name) $
      [ (fromString $ show $ Set.toList scenario, runScriptTest mprox (Proxy :: Proxy p) scenario)
      | scenario <- enumerateScenariosWhere condition
      ]

  runScriptTest ::
    (Proxy m) ->
    (Proxy p) ->
    Set p ->
    Property
  runScriptTest _ pprox targetProperties = property $ do
    (m :: m) <- parameterisedGenerator targetProperties
    case evaluateScript $ script pprox m of
      Left (EvaluationError logs err) -> deliverResult pprox m (Left (logs, err))
      Right res -> deliverResult pprox m (Right res)
      Left err -> footnoteShow err >> failure

  deliverResult ::
    MonadTest t =>
    (Proxy p) ->
    m ->
    Either ([Text], String) (ExBudget, [Text]) ->
    t ()
  deliverResult pprox model res =
    case (shouldPass, res) of
      (False, Left _) -> success
      (True, Right (cost, _)) -> successWithBudgetCheck cost
      (True, Left err) -> failWithFootnote $ unexpectedFailure err
      (False, Right (_, logs)) -> failWithFootnote $ unexpectedSuccess logs
    where
      shouldPass :: Bool
      shouldPass = satisfiesFormula (expect (Proxy :: Proxy m) pprox) $ properties model
      successWithBudgetCheck :: MonadTest t => ExBudget -> t ()
      successWithBudgetCheck cost@(ExBudget cpu mem) =
        if inInterval cpu (modelCPUBounds pprox model) && inInterval mem (modelMemoryBounds pprox model)
          then success
          else failWithFootnote $ budgetCheckFailure cost
            where inInterval :: Ord a => a -> (a,a) -> Bool
                  inInterval a (l,u) = a >= l && a <= u
      budgetCheckFailure :: ExBudget -> String
      budgetCheckFailure cost =
        renderStyle ourStyle $
          "Success! But at what cost?"
            $+$ hang "Lower Bound" 4 (ppDoc (ExBudget (fst (modelCPUBounds pprox model)) (fst (modelMemoryBounds pprox model))))
            $+$ hang "Actual Cost" 4 (ppDoc cost)
            $+$ hang "Upper Bound" 4 (ppDoc (ExBudget (snd (modelCPUBounds pprox model)) (snd (modelMemoryBounds pprox model))))
      unexpectedSuccess :: [Text] -> String
      unexpectedSuccess logs =
        renderStyle ourStyle $
          "Unexpected success" $+$ dumpState pprox logs
      unexpectedFailure :: ([Text], String) -> String
      unexpectedFailure (logs, reason) =
        renderStyle ourStyle $
          text ("Unexpected failure(" <> reason <> ")") $+$ dumpState pprox logs
      dumpState :: Proxy p -> [Text] -> Doc
      dumpState (Proxy :: Proxy p) logs =
        ""
          $+$ hang "Inputs" 4 dumpInputs
          $+$ hang "Logs" 4 (dumpLogs logs)
          $+$ hang "Expected " 4 (if shouldPass then "Pass" else "Fail")
          $+$ hang "Properties " 4 (ppDoc $ (properties model :: Set p))
      dumpInputs :: Doc
      dumpInputs =
        "Parameters"
          $+$ ppDoc model
      dumpLogs :: [Text] -> Doc
      dumpLogs logs = vcat . fmap go . zip [1 ..] $ logs
      go :: (Int, Text) -> Doc
      go (ix, line) = (int ix <> colon) <+> (text . show $ line)


failWithFootnote :: MonadTest m => String -> m a
failWithFootnote s = footnote s >> failure

ourStyle :: Style
ourStyle = style {lineLength = 80}
