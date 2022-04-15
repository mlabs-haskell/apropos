{-# LANGUAGE MultiWayIf #-}

module Spec.Rational (
  ratGenSelfTests,
  ratSampleTests,
  RatProp (..),
  Rat (..),
) where

import Apropos
import Spec.IntPermutationGen

import Control.Lens (lens)
import Data.Ratio
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup, testProperty)

-- Note: this is probably not an ideal way to model rational numbers
-- this example is used to ilustrate both trivial and non-trivial relations between
-- the model props and the submodel props
data Rat = Rational {num :: Int, den :: Int}
  deriving stock (Show, Generic)
  deriving anyclass (Hashable)

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
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel RatProp where
  logic =
    abstractionLogic @Rat
      :&&: (Var RatZero :->: Var RatSmall)
      :&&: ExactlyOne [Var RatSmall, Var RatLarge]
      :&&: (Var (Num IsSmall) :->: Var RatSmall)
      :&&: Not (Var $ Den IsZero)
      :&&: (Var (Num IsZero) :<->: Var RatZero)
      :&&: ExactlyOne [Var RatPos, Var RatZero, Var RatNeg]
      :&&: (Var RatZero :||: (ExactlyOne [Var (Num IsNegative), Var (Den IsNegative)] :<->: Var RatNeg))
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
          , productModelAbstraction = lens num (\r n -> r {num = n})
          }
    , WrapAbs $
        ProductAbstraction
          { abstractionName = "denominator"
          , propertyAbstraction = abstractsProperties Den
          , productModelAbstraction = lens den (\r d -> r {den = d})
          }
    ]

instance HasPermutationGenerator RatProp Rat where
  -- Some of the morphisms generated are nonsensicle ie. make Large >>> fix sign >>> fix small
  -- so we set this to true to disable the check that each morphism is usefull
  allowRedundentMorphisms _ = True

  generators =
    ( abstractionMorphisms
        ++ [ Morphism
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
      -- The relationship between RatNet, RatPos and RatZero and the sub model properties
      -- is simple and fully described by the model, so it's possible to fix the invalid morphisms like numerator of negate
      -- by simply branching to any rational sign and failing on the branches where the logic is not satisfied
      >>> [ Morphism
              { name = "fix sign"
              , match = Yes
              , contract =
                  removeAll [RatZero, RatPos, RatNeg]
                    >> branches
                      [ add RatZero >> matches logic
                      , add RatPos >> matches logic
                      , add RatNeg >> matches logic
                      ]
              , morphism = pure
              }
          ]
      >>>
      -- The RatLarge and rat small properties are less trivial
      -- they can be fixed with morphisms that don't change the properties
      --  but check the various cases and fix the model to satisfiy its properties
      [ Morphism
          { name = "fix large"
          , match = Var RatLarge
          , contract = pure ()
          , morphism = \r ->
              if satisfiesProperty RatLarge r
                then pure r
                else
                  let n = num r
                      d = den r
                      nl = satisfiesProperty IsLarge (num r)
                      dl = satisfiesProperty IsLarge (den r)
                      ns = not nl
                      ds = not dl
                   in if
                          | ns && ds -> pure $ Rational (10 * signum n) (signum d)
                          | nl && ds -> pure $ Rational (101 * signum n) d
                          | nl && dl ->
                              if n `elem` [minBound, maxBound]
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
                else
                  let n = num r
                      d = den r
                      nl = satisfiesProperty IsLarge (num r)
                      dl = satisfiesProperty IsLarge (den r)
                      ns = not nl
                      ds = not dl
                   in if
                          | ns && ds -> pure $ Rational (9 * signum n) d
                          | nl && ds -> do
                              let d' = max (abs d) 2
                              n' <- int (linear 11 (10 * d' - 1))
                              pure $ Rational (n' * signum n) (d' * signum d)
                          | nl && dl ->
                              if n `elem` [minBound, maxBound]
                                then do
                                  d' <- int (linear (maxBound `div` 10 + 1) (maxBound - 1))
                                  pure $ Rational n (d' * signum d)
                                else do
                                  d' <- int (linear 11 (maxBound `div` 10))
                                  n' <- int (linear 11 (10 * d' + 1))
                                  pure $ Rational (n' * signum n) (d' * signum d)
                          | otherwise -> error "unexpected model"
          }
      ]

instance HasParameterisedGenerator RatProp Rat where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen Rat
baseGen = Rational <$> genSatisfying @IntProp Yes <*> genSatisfying (Not $ Var IsZero)

ratGenSelfTests :: TestTree
ratGenSelfTests =
  testGroup "ratPermGenSelfTests" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: Morphism RatProp Rat) -> True)
        baseGen

ratSampleTests :: TestTree
ratSampleTests =
  testGroup
    "ratSampleTests"
    [testProperty "ratSampleTest" (sampleGenTest (Apropos :: Rat :+ RatProp))]
