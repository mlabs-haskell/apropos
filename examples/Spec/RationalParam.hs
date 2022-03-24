{-# LANGUAGE MultiWayIf #-}
module Spec.RationalParam
  (RatProp(..),
  ratGenSelfTests,
  )where

import Apropos
--import Apropos.Gen (int,linear)
import Spec.Int
import GHC.Generics
import Data.Ratio

--import Data.Set (Set)
import Data.Set as Set

import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (fromGroup)

data Rat = Rational{num :: Int,den :: Int}
  deriving stock Show

asRational :: Rat -> Rational
asRational (Rational n d) = fromIntegral n % fromIntegral d

data RatProp
  = RatPos
  | RatZero
  | RatNeg
  | RatLarge
  | RatSmall
  | Num IntProp
  | Den IntProp
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable)

instance LogicalModel RatProp where
  logic =
    (Num <$> logic) :&&: (Den <$> logic)
    :&&: (Var RatZero :->: Var RatSmall)
    :&&: ExactlyOne [Var RatSmall , Var RatLarge ]
    :&&: (Var (Num IsSmall) :->: Var RatSmall)
    :&&: Not (Var $ Den IsZero)
    :&&: (Var (Num IsZero) :<->: Var RatZero)
    :&&: ExactlyOne [ Var RatPos, Var RatZero, Var RatNeg ]
    :&&: (Not (Var RatZero) :->: (ExactlyOne [ Var (Num IsNegative) , Var (Den IsNegative)] :<->: Var RatNeg ))
    :&&: (((Var (Num IsMaxBound) :||: Var (Num IsMinBound)) :&&: Var RatSmall) :->: Var (Den IsLarge))
    :&&: ((Var (Den IsMaxBound) :||: Var (Den IsMinBound)) :->: Var RatSmall)

instance HasLogicalModel RatProp Rat where
  satisfiesProperty RatPos r = asRational r > 0
  satisfiesProperty RatZero r = asRational r == 0
  satisfiesProperty RatNeg r = asRational r < 0
  satisfiesProperty RatLarge r = abs (asRational r) > 10
  satisfiesProperty RatSmall r = abs (asRational r) <= 10
  satisfiesProperty (Num p) r = satisfiesProperty p (num r)
  satisfiesProperty (Den p) r = satisfiesProperty p (den r)

instance HasParameterisedGenerator RatProp Rat where
  parameterisedGenerator s = do
    let prop p = p `elem` s
    let numS = Set.fromList [ p | Num p <- Set.toList s ]
    let denS = Set.fromList [ p | Den p <- Set.toList s ]
    n <- parameterisedGenerator @IntProp @Int numS
    d <- parameterisedGenerator @IntProp @Int denS
    let r = Rational n d
    if | prop RatLarge && satisfiesProperty RatLarge r -> pure r
       | prop RatSmall && satisfiesProperty RatSmall r -> pure r
       | prop RatLarge && not (satisfiesProperty RatLarge r) ->
          if | prop (Num IsSmall) && prop (Den IsSmall) -> pure $ Rational (10 * signum n) (signum d)
             | prop (Num IsLarge) && prop (Den IsSmall) -> pure $ Rational (101 * signum n) d
             | prop (Num IsLarge) && prop (Den IsLarge) ->
               if | (abs d < maxBound `div` 10) -> do
                    n' <- int (linear (abs d * 10+1) maxBound)
                    pure $ Rational (n' * signum n) d
                  | prop (Num IsMaxBound) || prop (Num IsMinBound)-> do
                      d' <- int (linear 11 (maxBound `div` 10 - 1))
                      pure $ Rational n (d' * signum d)
                  | abs n > 101 -> do
                    d' <- int (linear 11 (abs n `div` 10 - 1))
                    pure $ Rational n (d' * signum d)
                  | otherwise -> error $ "unexpected model" ++ show s
             | otherwise -> error $ "unexpected model" ++ show s
       | prop RatSmall && not (satisfiesProperty RatSmall r) ->
          if | prop (Num IsSmall) && prop (Den IsSmall) -> pure $ Rational (9 * signum n) d
             | prop (Num IsLarge) && prop (Den IsSmall) -> do
               let d' = max (abs d) 2
               n' <- int (linear 11 (10*d'-1))
               pure $ Rational (n' * signum n) (d' * signum d)
             | prop (Num IsLarge) && prop (Den IsLarge) ->
               if | not (prop (Num IsMinBound)) && not (prop (Num IsMaxBound)) && (abs d > (maxBound -1) `div` 10) -> do
                      n' <- int (linear 10 (10*abs d+1))
                      pure $ Rational (n' * signum n) d
                  | prop (Num IsMaxBound) || prop (Num IsMinBound)-> do
                      d' <- int (linear (maxBound `div` 10 + 1) (maxBound -1))
                      pure $ Rational n (d' * signum d)
                  | abs n > 101 -> do
                      d' <- int (linear (abs n `div` 10 + 1) (maxBound -1))
                      pure $ Rational n (d' * signum d)
                  | otherwise -> do
                    d' <- int (linear 11 (maxBound `div` 10))
                    n' <- int (linear 11 (10*d'+1))
                    pure $ Rational (n' * signum n) (d' * signum d)
             | otherwise -> error $ "unexpected model" ++ show s
       | otherwise -> error $ "unexpected model " ++ show s

ratGenSelfTests :: TestTree
ratGenSelfTests =
    fromGroup
      $ runGeneratorTestsWhere
        (Apropos :: Rat :+ RatProp)
        "rat tests"
        Yes
