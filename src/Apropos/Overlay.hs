{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Apropos.Overlay (
  Overlay (..),
  soundOverlay,
) where

import Apropos.LogicalModel
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Hedgehog (Property, assert, property)

class (LogicalModel op, LogicalModel sp) => Overlay op sp | op -> sp where
  overlays :: op -> Formula sp

conectingLogic :: Overlay op sp => Formula (Either op sp)
conectingLogic = All [Var (Left op) :<->: (Right <$> overlays op) | op <- enumerated]

soundOverlay :: forall op sp. Overlay op sp => Property
soundOverlay = case solveAll (antiValidity @op @sp) of
  [] -> property $ assert True
  (violation : _) -> case (emptyOverlays @sp @op, uncoveredSubSolutions @sp @op) of
    (empty : _, _) ->
      error $
        "found solution to overlay with no coresponding sub model solutions\n"
          ++ show (Set.toList empty)
    (_, uncovered : _) ->
      error $
        "found solution to sub model which is excluded by overlay logic\n"
          ++ show (Set.toList uncovered)
    ([], []) ->
      error $
        "internal apropos error, found overlay inconsistancy but failed to find exact problem.\n"
          ++ "please report this as a bug\n"
          ++ "here's some info that might help (and which ideally should be included in the bug report):\n"
          ++ show violation

-- we want to assure (Left logic) === (conectingLogic :&&: (Right <$> logic))
antiValidity :: Overlay op sp => Formula (Either op sp)
antiValidity = conectingLogic :&&: ((Left <$> logic) :++: (Right <$> logic))

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
