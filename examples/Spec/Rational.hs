module Spec.Rational (
  ratGenSelfTests,
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
    :&&: (Not (Var RatZero) :->: (ExactlyOne [ Var (Num IsNegative) , Var (Den IsNegative)] :<->: Var RatNeg ))

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
    [ Morphism
      { name = "negate num"
      , match = Yes
      , contract = do
        branches
          [has (Num IsNegative) >> remove (Num IsNegative) >> add (Num IsPositive)
          ,has (Num IsPositive) >> remove (Num IsPositive) >> add (Num IsNegative)
          ]
        branches
          [has RatNeg >> remove RatNeg >> add RatPos
          ,has RatPos >> remove RatPos >> add RatNeg
          ]
      , morphism = \(Rational n d) -> pure $ Rational (negate n) d
      }
    , Morphism
      { name = "negate den"
      , match = Yes
      , contract = do
        branches
          [has (Den IsNegative) >> remove (Den IsNegative) >> add (Den IsPositive)
          ,has (Den IsPositive) >> remove (Den IsPositive) >> add (Den IsNegative)
          ]
        branches
          [has RatNeg >> remove RatNeg >> add RatPos
          ,has RatPos >> remove RatPos >> add RatNeg
          ]
      , morphism = \(Rational n d) -> pure $ Rational n (negate d)
      }
    , Morphism
      { name = "make zero"
      , match = Yes
      , contract = clear >> addAll [RatZero , RatSmall, Num IsZero , Num IsSmall, Den IsSmall, Den IsPositive ]
      , morphism = const $ pure $ Rational 0 1
      }
    , Morphism
      { name = "make one"
      , match = Yes
      , contract = clear >> addAll [RatPos, RatSmall, Num IsPositive , Num IsSmall, Den IsSmall, Den IsPositive ]
      , morphism = const $ pure $ Rational 1 1
      }
    , Morphism
      { name = "make large"
      , match = Var RatSmall
      , contract = remove RatSmall >> add RatLarge
      , morphism = \(Rational n d) ->
        -- 10 * d + n < maxBound <-> d < (maxBound - n) / 10
        if d < (maxBound - n) `div` 10
           then pure $ Rational (n+10*d) d
           else pure $ Rational 100 10
      }
    , Morphism
      { name = "small num"
      , match = Var $ Num IsLarge
      , contract = remove (Num IsLarge) >> add (Num IsSmall)
      , morphism = \(Rational _ d) -> do
        n <- genSatisfying (Var IsSmall)
        return $ Rational n d
      }
    , Morphism
      { name = "small den"
      , match = Var $ Den IsLarge
      , contract = remove (Den IsLarge) >> add (Den IsSmall)
      , morphism = \(Rational n _) -> do
        d <- genSatisfying (Var IsSmall)
        return $ Rational n d
      }
    ]
    -- ++ abstractionMorphisms

instance HasParameterisedGenerator RatProp Rat where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen Rat
baseGen = Rational <$> genSatisfying @IntProp Yes <*> genSatisfying (Not $ Var IsZero)


ratGenSelfTests :: TestTree
ratGenSelfTests =
  testGroup "ratGenSelfTests" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: Morphism RatProp Rat) -> True)
        baseGen
