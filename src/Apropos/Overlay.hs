{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Apropos.Overlay (
  Overlay (..),
  soundOverlay,
  overlaySources,
  deduceFromOverlay,
) where

import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.HasPermutationGenerator.Source
import Apropos.LogicalModel
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Hedgehog (Property, failure, footnote, property)

class (LogicalModel op, LogicalModel sp) => Overlay op sp | op -> sp where
  overlays :: op -> Formula sp

soundOverlay :: forall op sp. Overlay op sp => Property
soundOverlay = property $
  case solveAll (antiValidity @op @sp) of
    (violation : _) -> do
      case uncoveredSubSolutions @sp @op of
        (uncovered : _) ->
          footnote $
            "found solution to sub model which is excluded by overlay logic\n"
              ++ show (Set.toList uncovered)
        [] ->
          footnote $
            "internal apropos error, found overlay inconsistancy but failed to find exact problem.\n"
              ++ "please report this as a bug\n"
              ++ "here's some info that might help "
              ++ "(and which ideally should be included in the bug report):\n"
              ++ show violation
      failure
    [] -> case emptyOverlays @sp @op of
      (empty : _) -> do
        footnote $
          "found solution to overlay with no coresponding sub model solutions\n"
            ++ show (Set.toList empty)
        failure
      [] -> pure ()

conectingLogic :: Overlay op sp => Formula (Either op sp)
conectingLogic = All [Var (Left op) :<->: (Right <$> overlays op) | op <- enumerated]

-- we want to assure: conectingLogic => (Left <$> logic) === (Right <$> logic)
antiValidity :: Overlay op sp => Formula (Either op sp)
antiValidity =
  let overlayLogic = Left <$> logic
      subModelLogic = Right <$> logic
   in Not ((conectingLogic :&&: subModelLogic) :->: overlayLogic)

-- list of solutions to the overlay logic which have no coresponding solutions in the sub-model
-- if this is not empty that is considered unsound
emptyOverlays :: forall sp op. Overlay op sp => [Set op]
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
uncoveredSubSolutions :: forall sp op. Overlay op sp => [Set sp]
uncoveredSubSolutions =
  let sols :: [Map sp Bool]
      sols = solveAll (logic :: Formula sp)
      solFormulas :: [(Set sp, Formula sp)]
      solFormulas = [(Map.keysSet $ Map.filter id sol, All [if b then Var sp else Not (Var sp) | (sp, b) <- Map.toList sol]) | sol <- sols]
   in [ ssp | (ssp, form) <- solFormulas, null $
                                            solveAll $
                                              (Right <$> form) :&&: (Left <$> logic @op) :&&: conectingLogic
      ]

overlaySources :: (Overlay p op, HasParameterisedGenerator op m) => [Source p m]
overlaySources =
  [ Source
      { sourceName = "overlay"
      , covers = Yes
      , pgen = \ps -> genSatisfying (All [(if p `elem` ps then id else Not) $ overlays p | p <- enumerated])
      }
  ]

deduceFromOverlay :: (HasLogicalModel sp m, Overlay op sp) => op -> m -> Bool
deduceFromOverlay = satisfiesExpression . overlays
