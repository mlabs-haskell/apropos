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
  sourceAbstractions =
    [ SoAs $
        SourceAbstraction
          { sourceAbsName = "make rat"
          , constructor = Rational
          , productAbs =
              ProductAbstraction
                { abstractionName = "numerator"
                , propertyAbstraction = abstractsProperties Num
                , productModelAbstraction = lens num (\r n -> r {num = n})
                }
                :& ProductAbstraction
                  { abstractionName = "denominator"
                  , propertyAbstraction = abstractsProperties Den
                  , productModelAbstraction = lens den (\r d -> r {den = d})
                  }
                :& Nil
          }
    ]
  sourceCorections =
    -- the sub models don't know about rat large and rat small
    -- so they will generate invalid models with respect to these properties
    -- if they were always large or always small you could just restrict the source
    -- and create a morphism but because they can be either you need Corrections which
    -- take the source output and ensure it has the apropriate properties
    [ Correction
        { corName = "large"
        , domain = Var RatLarge
        , modifier = \r ->
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
    , Correction
        { corName = "small"
        , domain = Var RatSmall
        , modifier = \r ->
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

instance HasPermutationGenerator RatProp Rat where
  -- Some of the morphisms generated are nonsensicle ie. make Large >>> fix sign >>> fix small
  -- so we set this to true to disable the check that each morphism is usefull
  allowRedundentMorphisms = True
  sources = abstractionSources

  generators =
    abstractionMorphisms
      -- the negate morphisms will violate the model logic on their own becasue they don't
      -- know about the rat sign properties, apending a morphism that re-deduces the sign
      -- from the model logic fixes this
      >>> [ Morphism
              { name = "fix sign"
              , match = Yes
              , contract =
                  removeAll [RatZero, RatPos, RatNeg]
                    >> branches
                      [ add RatZero
                      , add RatPos
                      , add RatNeg
                      ]
              , morphism = pure
              }
          ]

instance HasParameterisedGenerator RatProp Rat where
  parameterisedGenerator = buildGen

ratGenSelfTests :: TestTree
ratGenSelfTests =
  testGroup "ratPermGenSelfTests" $
    pure $
      fromGroup $
        permutationGeneratorSelfTest @RatProp

ratSampleTests :: TestTree
ratSampleTests =
  testGroup
    "ratSampleTests"
    [testProperty "ratSampleTest" (sampleGenTest @RatProp)]
