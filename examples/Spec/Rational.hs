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

instance HasPermutationGenerator RatProp Rat where
  -- Some of the morphisms generated are nonsensicle ie. make Large >>> fix sign >>> fix small
  -- so we set this to true to disable the check that each morphism is usefull
  allowRedundentMorphisms = True
  sources = abstractionSources

  generators =
    ( abstractionMorphisms
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
    )
      ++ [ Morphism
            { name = "make large (large,small)"
            , match = Var (Num IsLarge) :&&: Var (Num IsPositive) :&&: Var (Den IsSmall) :&&: Var RatSmall
            , contract = remove RatSmall >> add RatLarge
            , morphism = \r -> pure $ r {num = 101}
            }
         , Morphism
            { name = "make small (large,small)"
            , match = Var (Num IsLarge) :&&: Var (Num IsPositive) :&&: Var (Den IsSmall) :&&: Var (Den IsPositive) :&&: Var RatLarge
            , contract = remove RatLarge >> add RatSmall
            , morphism = \r -> do
                let d' = max 2 (den r)
                n' <- int $ linear 11 (10 * d')
                pure $ Rational n' d'
            }
         , Morphism
            { name = "make large (Max,large)"
            , match = Var (Num IsMaxBound) :&&: Var (Den IsLarge) :&&: Var (Den IsPositive) :&&: Var RatSmall
            , contract = remove RatSmall >> add RatLarge
            , morphism = \r -> do
                d' <- int $ linear 11 (maxBound `div` 10 - 1)
                pure $ r {den = d'}
            }
         , Morphism
            { name = "make small (Max,large)"
            , match = Var (Num IsMaxBound) :&&: Var (Den IsLarge) :&&: Var (Den IsPositive) :&&: Var RatLarge
            , contract = remove RatLarge >> add RatSmall
            , morphism = \r -> do
                d' <- int $ linear (maxBound `div` 10 + 1) (maxBound - 1)
                pure $ r {den = d'}
            }
         , Morphism
            { name = "make large (Min,large)"
            , match = Var (Num IsMinBound) :&&: Var (Den IsLarge) :&&: Var (Den IsPositive) :&&: Var RatSmall
            , contract = remove RatSmall >> add RatLarge
            , morphism = \r -> do
                d' <- int $ linear 11 (maxBound `div` 10 - 1)
                pure $ r {den = d'}
            }
         , Morphism
            { name = "make small (Min,large)"
            , match = Var (Num IsMinBound) :&&: Var (Den IsLarge) :&&: Var (Den IsPositive) :&&: Var RatLarge
            , contract = remove RatLarge >> add RatSmall
            , morphism = \r -> do
                d' <- int $ linear (maxBound `div` 10 + 1) (maxBound - 1)
                pure $ r {den = d'}
            }
         -- TODO these last 2 are just sources
         -- but the broader source wouldn't be connected if they were made sources
         , Morphism
            { name = "make large (large,large)"
            , match = Var (Num IsLarge) :&&: Not (Var (Num IsMaxBound)) :&&: Var (Num IsPositive) :&&: Var (Den IsLarge) :&&: Var (Den IsPositive) :&&: Var RatSmall
            , contract = remove RatSmall >> add RatLarge
            , morphism = \_r -> do
                n' <- int (linear 111 (maxBound - 1))
                d' <- int (linear 11 (n' `div` 10))
                pure $ Rational n' d'
            }
         , Morphism
            { name = "make small (large,large)"
            , match = Var (Num IsLarge) :&&: Not (Var (Num IsMaxBound)) :&&: Var (Num IsPositive) :&&: Var (Den IsLarge) :&&: Var (Den IsPositive) :&&: Var RatLarge
            , contract = remove RatLarge >> add RatSmall
            , morphism = \_r -> do
                d' <- int (linear 11 (maxBound `div` 10))
                n' <- int (linear 11 (10 * d' - 1))
                pure $ Rational n' d'
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
