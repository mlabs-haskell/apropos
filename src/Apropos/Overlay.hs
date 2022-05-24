module Apropos.Overlay (
  Overlay (..),
  soundOverlay,
  overlaySources,
  deduceFromOverlay,
) where

import Apropos.Error
import Apropos.HasParameterisedGenerator
import Apropos.HasPermutationGenerator.Source
import Apropos.Logic (Formula (..), Strategy (logic, universe), satisfiesExpression, scenarios, solveAll)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Hedgehog (Property, failure, footnote, property)

class (Strategy op o, Strategy sp s) => Overlay op sp o s | op -> sp where
  overlays :: op -> Formula sp

soundOverlay :: forall op sp o s. (Ord sp, Ord op, Show sp, Show op, Overlay op sp o s) => Property
soundOverlay = property $
  case solveAll (antiValidity @op @sp) of
    (violation : _) -> do
      case uncoveredSubSolutions @sp @op of
        (uncovered : _) ->
          footnote $
            "found solution to sub model which is excluded by overlay logic\n"
              ++ show (Set.toList uncovered)
        [] -> internalError $ "overlay violation" ++ show violation
      failure
    [] -> case emptyOverlays @sp @op of
      (empty : _) -> do
        footnote $
          "found solution to overlay with no coresponding sub model solutions\n"
            ++ show (Set.toList empty)
        failure
      [] -> pure ()

conectingLogic :: (Overlay op sp o p) => Formula (Either op sp)
conectingLogic = All [Var (Left op) :<->: (Right <$> overlays op) | op <- universe]

-- we want to assure: conectingLogic => (Left <$> logic) === (Right <$> logic)
antiValidity :: (Overlay op sp o p) => Formula (Either op sp)
antiValidity =
  let overlayLogic = Left <$> logic
      subModelLogic = Right <$> logic
   in Not ((conectingLogic :&&: subModelLogic) :->: overlayLogic)

-- list of solutions to the overlay logic which have no coresponding solutions in the sub-model
-- if this is not empty that is considered unsound
emptyOverlays :: forall sp op o s. (Ord sp, Ord op, Overlay op sp o s) => [Set op]
emptyOverlays =
  let sols :: [Map op Bool]
      sols = solveAll (logic :: Formula op)
      solFormulas :: [(Set op, Formula op)]
      solFormulas = [(Map.keysSet $ Map.filter id sol, All [if b then Var sp else Not (Var sp) | (sp, b) <- Map.toList sol]) | sol <- sols]
   in [ sop | (sop, form) <- solFormulas, null $
                                            solveAll $
                                              (Left <$> form) :&&: (Right <$> logic @sp) :&&: conectingLogic
      ]

-- list of sub model solutions that aren't covered by any solutions to the overlaying model
-- if this is not empty that is also unsound
uncoveredSubSolutions :: forall sp op s o. (Ord sp, Ord op, Overlay op sp o s) => [Set sp]
uncoveredSubSolutions =
  let sols :: [Map sp Bool]
      sols = solveAll (logic :: Formula sp)
      solFormulas :: [(Set sp, Formula sp)]
      solFormulas = [(Map.keysSet $ Map.filter id sol, All [if b then Var sp else Not (Var sp) | (sp, b) <- Map.toList sol]) | sol <- sols]
   in [ ssp | (ssp, form) <- solFormulas, null $
                                            solveAll $
                                              (Right <$> form) :&&: (Left <$> logic @op) :&&: conectingLogic
      ]

overlaySources :: (Ord op, Show op, Ord p, Overlay p op o m, HasParameterisedGenerator op m) => [Source p m]
overlaySources =
  [ Source
    { sourceName = "overlay"
    , covers = All [if p `elem` ps then Var p else Not (Var p) | p <- universe]
    , gen = genSatisfying (All [(if p `elem` ps then id else Not) $ overlays p | p <- universe])
    }
  | ps <- scenarios
  ]

deduceFromOverlay :: (Ord sp, Strategy sp m, Overlay op sp o s) => op -> m -> Bool
deduceFromOverlay = satisfiesExpression . overlays
