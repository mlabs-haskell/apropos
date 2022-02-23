{-# LANGUAGE TemplateHaskell #-}

module Spec.IntPair (
  intPairGenTests,
  intPairGenPureTests,
  intPairGenSelfTests,
  intPairGenPlutarchTests,
  ) where
import Spec.IntPermutationGen
import Apropos.HasLogicalModel
import Apropos.LogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.HasPermutationGenerator
import Apropos.Gen
import Apropos.Pure
import Apropos.Script
import Data.Proxy (Proxy(..))
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import Plutarch (compile)
import Plutarch.Prelude
import Control.Monad (join)

data IntPairProp =
  L IntProp | R IntProp
  deriving stock (Eq,Ord,Show)

$(gen_enumerable ''IntPairProp)

instance LogicalModel IntPairProp where
  logic = (L <$> logic) :&&: (R <$> logic)

instance HasLogicalModel IntPairProp (Int,Int) where
  satisfiesProperty (L p) (i,_) = satisfiesProperty p i
  satisfiesProperty (R p) (_,i) = satisfiesProperty p i

instance HasPermutationGenerator IntPairProp (Int,Int) where
  generators =
 --TODO liftEdges could be prettier - we could use lenses to
 --                                 -            get and set a substructure in the model
 --                                 -            Maybe extract a subproperty from a property
 --
 --                                 - we could derive the prefix string from the property constructor
 --
 --                                 - then we would have a 4 argument function instead of a 6 argument function
    let l = liftEdges L
                      fst
                      (\f (_,r') -> (f,r'))
                      (\p -> case p of
                               (L q) -> Just q
                               _ -> Nothing)
                      "L"
                      generators
        r = liftEdges R
                      snd
                      (\f (l',_) -> (l',f))
                      (\p -> case p of
                               (R q) -> Just q
                               _ -> Nothing)
                      "R"
                      generators

     in join [l,r]

instance HasParameterisedGenerator IntPairProp (Int,Int) where
  parameterisedGenerator = buildGen baseGen

baseGen :: PGen (Int,Int)
baseGen = (,) <$> genSatisfying (Yes :: Formula IntProp)
              <*> genSatisfying (Yes :: Formula IntProp)

intPairGenTests :: TestTree
intPairGenTests = testGroup "Spec.IntPermutationGen" $
    fromGroup <$> [
      runGeneratorTestsWhere (Proxy :: Proxy (Int,Int)) "(Int,Int) Generator" (Yes :: Formula IntPairProp)
    ]

instance HasPureRunner IntPairProp (Int,Int) where
  expect _ = All $ Var <$> (join [L <$> [IsSmall,IsNegative]
                                 ,R <$> [IsSmall,IsPositive]
                                 ])
  script _ (l,r) = l < 0 && l >= -10 && r > 0 && r <= 10

intPairGenPureTests :: TestTree
intPairGenPureTests = testGroup "Pure.IntPair" $
  fromGroup <$> [

    runPureTestsWhere (Proxy :: Proxy (Int,Int)) "AcceptsLeftSmallNegativeRightSmallPositive" (Yes :: Formula IntPairProp)
                ]

instance HasScriptRunner IntPairProp (Int,Int) where
  expect _ _ =  All $ Var <$> (join [L <$> [IsSmall,IsNegative]
                                 ,R <$> [IsSmall,IsPositive]])
  script _ (il,ir) =
    let iil = (fromIntegral il) :: Integer
        iir = (fromIntegral ir) :: Integer
     in compile (pif ((pif (((fromInteger iil) #< ((fromInteger 0) :: Term s PInteger)) #&& (((fromInteger (-10)) :: Term s PInteger) #<= (fromInteger iil))) (pcon PTrue) perror) #&& (pif ((((fromInteger 0) :: Term s PInteger) #< (fromInteger iir)) #&& ((fromInteger iir) #<= ((fromInteger (10)) :: Term s PInteger))) (pcon PTrue) perror)) (pcon PUnit) perror)


intPairGenPlutarchTests :: TestTree
intPairGenPlutarchTests = testGroup "Plutarch.IntPair" $
  fromGroup <$> [
    runScriptTestsWhere (Proxy :: Proxy (Int,Int)) (Proxy :: Proxy IntPairProp) "AcceptsLeftSmallNegativeRightSmallPositive" Yes
  ]

intPairGenSelfTests :: TestTree
intPairGenSelfTests = testGroup "(Int,Int) HasPermutationGenerator permutationGeneratorSelfTest" $
  fromGroup <$> permutationGeneratorSelfTest True
               (\(_ :: PermutationEdge IntPairProp (Int,Int)) -> True) baseGen


