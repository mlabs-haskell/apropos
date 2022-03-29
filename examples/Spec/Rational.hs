{-# LANGUAGE MultiWayIf #-}
module Spec.Rational (
  ratPermGenSelfTests,
  RatProp(..),
  Rat(..),
                     ) where

import Apropos
import Spec.IntPermutationGen

import GHC.Generics
import Control.Lens (lens)
import Data.Ratio

import Test.Tasty (TestTree, testGroup)
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
  logic = abstractionLogic @Rat
    :&&: (Var RatZero :->: Var RatSmall)
    :&&: ExactlyOne [Var RatSmall , Var RatLarge ]
    :&&: (Var (Num IsSmall) :->: Var RatSmall)
    :&&: Not (Var $ Den IsZero)
    :&&: (Var (Num IsZero) :<->: Var RatZero)
    :&&: ExactlyOne [ Var RatPos, Var RatZero, Var RatNeg ]
    :&&: (Var RatZero :||: (ExactlyOne [ Var (Num IsNegative) , Var (Den IsNegative)] :<->: Var RatNeg ))
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

instance HasAbstractions RatProp Rat where
  abstractions =
    [ WrapAbs $
      ProductAbstraction
        { abstractionName = "numerator"
        , propertyAbstraction = abstractsProperties Num
        , productModelAbstraction = lens num (\r n -> r{num=n})
        }
    , WrapAbs $
      ProductAbstraction
        { abstractionName = "denominator"
        , propertyAbstraction = abstractsProperties Den
        , productModelAbstraction = lens den (\r d -> r{den=d})
        }
    ]

instance HasPermutationGenerator RatProp Rat where
  generators =
    (abstractionMorphisms
      ++
        [ Morphism
          { name = "make Large"
          , match = Not $ Var RatLarge
          , contract = remove RatSmall >> add RatLarge
          , morphism = pure
          }
        , Morphism
          { name = "make Small"
          , match = Not $ Var RatSmall
          , contract = remove RatLarge >> add RatSmall
          , morphism = pure
          }
        ]
    )
        >>>
        [
         Morphism
         { name = "fix sign"
         , match = Yes
         , contract = removeAll [RatZero,RatPos,RatNeg] >> branches
           [ add RatZero >> matches logic
           , add RatPos >> matches logic
           , add RatNeg >> matches logic
           ]
         , morphism = pure
         }
        ] >>>
        [ Morphism
          { name = "fix large"
          , match = Var RatLarge
          , contract = pure ()
          , morphism = \r ->
            if satisfiesProperty RatLarge r
               then pure r
               else let
                n = num r
                d = den r
                nl = satisfiesProperty IsLarge (num r)
                dl = satisfiesProperty IsLarge (den r)
                ns = not nl
                ds = not dl
             in if | ns && ds -> pure $ Rational (10*signum n) (signum d)
                   | nl && ds -> pure $ Rational (101*signum n) d
                   | nl && dl ->
                     if n `elem` [minBound,maxBound]
                        then do
                          d' <- int (linear 11 (maxBound `div` 10))
                          pure $ Rational n (d' * signum d)
                        else do
                          n' <- int (linear 111 maxBound)
                          d' <- int (linear 11 (n' `div` 10))
                          pure $ Rational (n' * signum n) (d' * signum d)
                   | otherwise -> error "unexpected model"
          }
        , Morphism
          { name = "fix small"
          , match = Var RatSmall
          , contract = pure ()
          , morphism = \r ->
            if satisfiesProperty RatSmall r
               then pure r
               else let
                n = num r
                d = den r
                nl = satisfiesProperty IsLarge (num r)
                dl = satisfiesProperty IsLarge (den r)
                ns = not nl
                ds = not dl
             in if | ns && ds -> pure $ Rational (9*signum n) d
                   | nl && ds -> do
                     let d' = max (abs d) 2
                     n' <- int (linear 11 (10*d'-1))
                     pure $ Rational (n' * signum n) (d' * signum d)
                   | nl && dl ->
                     if n `elem` [minBound,maxBound]
                        then do
                          d' <- int (linear (maxBound `div` 10 + 1) (maxBound -1))
                          pure $ Rational n (d' * signum d)
                        else do
                          d' <- int (linear 11 (maxBound `div` 10))
                          n' <- int (linear 11 (10*d'+1))
                          pure $ Rational (n' * signum n) (d' * signum d)
                   | otherwise -> error "unexpected model"
          }
        ]

instance HasParameterisedGenerator RatProp Rat where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen Rat
baseGen = Rational <$> genSatisfying @IntProp Yes <*> genSatisfying (Not $ Var IsZero)


ratPermGenSelfTests :: TestTree
ratPermGenSelfTests =
  testGroup "ratPermGenSelfTests" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: Morphism RatProp Rat) -> True)
        baseGen
