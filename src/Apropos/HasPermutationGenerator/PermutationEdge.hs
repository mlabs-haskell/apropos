{-# LANGUAGE RankNTypes #-}

module Apropos.HasPermutationGenerator.PermutationEdge (
  PermutationEdge (..),
) where

import Apropos.Gen
import Apropos.HasPermutationGenerator.Contract
import Apropos.LogicalModel.Formula

data PermutationEdge p m = PermutationEdge
  { name :: String
  , match :: Formula p
  , contract :: Contract p ()
  , permuteGen :: Gen m m
  }

instance Eq (PermutationEdge p m) where
  (==) a b = name a == name b

instance Show (PermutationEdge p m) where
  show = name
