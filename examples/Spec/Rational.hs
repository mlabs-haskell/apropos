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
    :&&: (Not (Var RatZero) :->: (ExactlyOne [ Var (Num IsNegative) , Var (Den IsNegative)] :<->: Var RatNeg ))
    :&&: (((Var (Num IsMaxBound) :||: Var (Num IsMinBound)) :&&: Var RatSmall) :->: Var (Den IsLarge))

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
      , match = Not (Var (Den IsMinBound)) :&&: Not (Var (Den IsMaxBound))
      , contract = do
        branches
          [ has (Den IsNegative) >> remove (Den IsNegative) >> add (Den IsPositive)
          , has (Den IsPositive) >> remove (Den IsPositive) >> add (Den IsNegative)
          ]
        branches
          [ has RatNeg >> remove RatNeg >> add RatPos
          , has RatPos >> remove RatPos >> add RatNeg
          , has RatZero
          ]
      , morphism = \(Rational n d) ->
        let d' = negate d
          in if d' == maxBound
                then pure $ Rational n (d'-1)
                else pure $ Rational n d'
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
      { name = "small rat small pos num"
      , match = Var (Num IsLarge) :&&: Var RatSmall
      , contract = removeAll (Num <$> enumerated) >> addAll [Num IsSmall,Num IsPositive]
      , morphism = \(Rational _ d) -> do
        n <- genSatisfying (Var IsSmall :&&: Var IsPositive)
        return $ Rational n d
      }
    , Morphism
      { name = "large rat small pos den"
      , match = Var (Den IsLarge) :&&: Var RatLarge
      , contract = removeAll (Den <$> enumerated) >> addAll [Den IsSmall,Den IsPositive]
      , morphism = \(Rational n _) -> do
        d <- genSatisfying (Var IsSmall :&&: Var IsPositive)
        return $ Rational n d
      }
    , Morphism
      { name = "large rat large pos num"
      , match = Var (Num IsSmall) :&&: Var RatLarge
      , contract = removeAll (Num <$> enumerated) >> addAll [Num IsLarge,Num IsPositive]
      , morphism = \(Rational _ d) -> do
        n <- genSatisfying (Var IsLarge :&&: Var IsPositive :&&: Not (Var IsMaxBound))
        return $ Rational n d
      }
    , Morphism
      { name = "small rat large pos num"
      , match = Var (Num IsSmall) :&&: Var RatSmall
      , contract = removeAll (Num <$> enumerated) >> addAll [Num IsLarge,Num IsPositive]
      , morphism = \(Rational _ d) -> do
        n <- genSatisfying (Var IsLarge :&&: Var IsPositive :&&: Not (Var IsMaxBound))
        return $ Rational n d
      }
    , Morphism
      { name = "small rat large pos den"
      , match = Var (Den IsSmall) :&&: Var RatSmall
      , contract = removeAll (Den <$> enumerated) >> addAll [Den IsLarge,Den IsPositive]
      , morphism = \(Rational n _) -> do
        d <- genSatisfying (Var IsLarge :&&: Var IsPositive :&&: Not (Var IsMaxBound))
        return $ Rational n d
      }
    , Morphism
      { name = "large rat large pos den"
      , match = Var (Den IsSmall) :&&: Var RatLarge
      , contract = removeAll (Den <$> enumerated) >> addAll [Den IsLarge,Den IsPositive]
      , morphism = \(Rational n _) -> do
        d <- genSatisfying (Var IsLarge :&&: Var IsPositive :&&: Not (Var IsMaxBound))
        return $ Rational n d
      }
    , Morphism
      { name = "make den minBound"
      , match = Yes
      , contract = removeAll (Den <$> enumerated) >> addAll [Den IsNegative,Den IsLarge,Den IsMinBound]
      , morphism = \(Rational n _) -> do
        return $ Rational n minBound
      }
    , Morphism
      { name = "make num minBound"
      , match = Yes
      , contract = removeAll (Num <$> enumerated) >> addAll [Num IsNegative,Num IsLarge,Num IsMinBound]
      , morphism = \(Rational _ d) -> do
        return $ Rational minBound d
      }
    , Morphism
      { name = "make den maxBound"
      , match = Yes
      , contract = removeAll (Den <$> enumerated) >> addAll [Den IsPositive,Den IsLarge,Den IsMaxBound]
      , morphism = \(Rational n _) -> do
        return $ Rational n maxBound
      }
    , Morphism
      { name = "make num maxBound"
      , match = Yes
      , contract = removeAll (Num <$> enumerated) >> addAll [Num IsPositive,Num IsLarge,Num IsMaxBound]
      , morphism = \(Rational _ d) -> do
        return $ Rational maxBound d
      }
    ]
    -- ++ abstractionMorphisms

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
