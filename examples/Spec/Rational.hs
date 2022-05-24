module Spec.Rational (
  ratGenSelfTests,
  ratSampleTests,
  RatProp (..),
  Rat (..),
) where

import Apropos
import Apropos.LogicalModel
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
    (unProp <$> abstractionLogic @Rat)
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

instance HasAbstractions (Prop RatProp) Rat where
  sourceAbstractions =
    [ SoAs $
        SourceAbstraction
          { sourceAbsName = "make rat"
          , constructor = Rational
          , productAbs =
              ProductAbstraction
                { abstractionName = "numerator"
                , propertyAbstraction = abstractsProperties (Prop . Num . unProp)
                , productModelAbstraction = lens num (\r n -> r {num = n})
                }
                :& ProductAbstraction
                  { abstractionName = "denominator"
                  , propertyAbstraction = abstractsProperties (Prop . Den . unProp)
                  , productModelAbstraction = lens den (\r d -> r {den = d})
                  }
                :& Nil
          }
    ]

instance HasPermutationGenerator (Prop RatProp) Rat where
  sources =
    -- the morphisms that would make this source strongly conected end up looking more like sources
    -- so I filter it out and add the sub sources
    filter (("make rat over [numerator of Large,denominator of Large]" /=) . sourceName) abstractionSources
      ++ [ Source
            { sourceName = "source large (large,large)"
            , covers =
                Prop <$> Var (Num IsLarge)
                  :&&: Not (Var (Num IsMaxBound))
                  :&&: Var (Num IsPositive)
                  :&&: Var (Den IsLarge)
                  :&&: Var (Den IsPositive)
                  :&&: Var RatLarge
            , gen = do
                n' <- int (linear 111 (maxBound - 1))
                d' <- int (linear 11 (n' `div` 10))
                pure $ Rational n' d'
            }
         , Source
            { sourceName = "source small (large,large)"
            , covers =
                Prop <$> Var (Num IsLarge)
                  :&&: Not (Var (Num IsMaxBound))
                  :&&: Var (Num IsPositive)
                  :&&: Var (Den IsLarge)
                  :&&: Var (Den IsPositive)
                  :&&: Not (Var (Den IsMaxBound))
                  :&&: Var RatSmall
            , gen = do
                d' <- int (linear 11 (maxBound `div` 10))
                n' <- int (linear 11 (10 * d' - 1))
                pure $ Rational n' d'
            }
         ]

  generators =
    ( abstractionMorphisms
        -- the negate morphisms will violate the model logic on their own becasue they don't
        -- know about the rat sign properties, apending a morphism that deduces them
        -- from the model logic fixes this
        >>> [ Morphism
                { name = "fix sign"
                , match = Yes
                , contract = deduce (map Prop [RatZero, RatPos, RatNeg])
                , morphism = pure
                }
            ]
    )
      -- thse morphisms are needed to strongly connect the sources which contain both the
      -- rat large and rat small solutions
      ++ [ Morphism
            { name = "make large (large,small)"
            , match = Prop <$> Var (Num IsLarge) :&&: Var (Num IsPositive) :&&: Var (Den IsSmall) :&&: Var RatSmall
            , contract = remove (Prop RatSmall) >> add (Prop RatLarge)
            , morphism = \r -> pure $ r {num = 101}
            }
         , Morphism
            { name = "make small (large,small)"
            , match = Prop <$> Var (Num IsLarge) :&&: Var (Num IsPositive) :&&: Var (Den IsSmall) :&&: Var (Den IsPositive) :&&: Var RatLarge
            , contract = remove (Prop RatLarge) >> add (Prop RatSmall)
            , morphism = \r -> do
                let d' = max 2 (den r)
                n' <- int $ linear 11 (10 * d')
                pure $ Rational n' d'
            }
         , Morphism
            { name = "make large (Max,large)"
            , match = Prop <$> Var (Num IsMaxBound) :&&: Var (Den IsLarge) :&&: Var (Den IsPositive) :&&: Var RatSmall
            , contract = remove (Prop RatSmall) >> add (Prop RatLarge)
            , morphism = \r -> do
                d' <- int $ linear 11 (maxBound `div` 10 - 1)
                pure $ r {den = d'}
            }
         , Morphism
            { name = "make small (Max,large)"
            , match = Prop <$> Var (Num IsMaxBound) :&&: Var (Den IsLarge) :&&: Var (Den IsPositive) :&&: Var RatLarge
            , contract = remove (Prop RatLarge) >> add (Prop RatSmall)
            , morphism = \r -> do
                d' <- int $ linear (maxBound `div` 10 + 1) (maxBound - 1)
                pure $ r {den = d'}
            }
         , Morphism
            { name = "make large (Min,large)"
            , match = Prop <$> Var (Num IsMinBound) :&&: Var (Den IsLarge) :&&: Var (Den IsPositive) :&&: Var RatSmall
            , contract = remove (Prop RatSmall) >> add (Prop RatLarge)
            , morphism = \r -> do
                d' <- int $ linear 11 (maxBound `div` 10 - 1)
                pure $ r {den = d'}
            }
         , Morphism
            { name = "make small (Min,large)"
            , match = Prop <$> Var (Num IsMinBound) :&&: Var (Den IsLarge) :&&: Var (Den IsPositive) :&&: Var RatLarge
            , contract = remove (Prop RatLarge) >> add (Prop RatSmall)
            , morphism = \r -> do
                d' <- int $ linear (maxBound `div` 10 + 1) (maxBound - 1)
                pure $ r {den = d'}
            }
         ]

instance HasParameterisedGenerator (Prop RatProp) Rat where
  parameterisedGenerator = buildGen

ratGenSelfTests :: TestTree
ratGenSelfTests =
  testGroup "ratPermGenSelfTests" $
    pure $
      fromGroup $
        permutationGeneratorSelfTest @(Prop RatProp)

ratSampleTests :: TestTree
ratSampleTests =
  testGroup
    "ratSampleTests"
    [testProperty "ratSampleTest" (sampleGenTest @(Prop RatProp))]
