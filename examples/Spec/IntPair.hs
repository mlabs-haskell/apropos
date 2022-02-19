{-# LANGUAGE TemplateHaskell #-}

module Spec.IntPair (
  intPairGenTests,
  intPairGenPureTests,
  intPairGenSelfTests,
  ) where
import Spec.IntPermutationGen
import Proper.HasLogicalModel
import Proper.LogicalModel
import Proper.HasParameterisedGenerator
import Proper.HasPureTestRunner
import Proper.HasPermutationGenerator
--import Proper.HasPermutationGenerator.Contract
--import Proper.HasPermutationGenerator.Gen
--import Proper.HasPlutusTestRunner
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (linear)
import Data.Proxy (Proxy(..))
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)
--import Control.Monad.Trans.Reader (ask)
--import Plutarch (compile)
--import Plutarch.Prelude
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

baseGen :: Gen (Int,Int)
baseGen = (,) <$> Gen.int (linear minBound maxBound)
              <*> Gen.int (linear minBound maxBound)

intPairGenTests :: TestTree
intPairGenTests = testGroup "Spec.IntPermutationGen" $
    fromGroup <$> [
      runGeneratorTestsWhere (Proxy :: Proxy (Int,Int)) "(Int,Int) Generator" (Yes :: Formula IntPairProp)
    ]

instance HasPureTestRunner IntPairProp (Int,Int) where
  expect _ = All $ Var <$> (join [L <$> [IsSmall,IsNegative]
                                 ,R <$> [IsSmall,IsPositive]
                                 ])
  script _ (l,r) = l < 0 && l >= -10 && r > 0 && r <= 10

intPairGenPureTests :: TestTree
intPairGenPureTests = testGroup "Pure.AcceptsSmallNegativeInts" $
  fromGroup <$> [

    runPureTestsWhere (Proxy :: Proxy Int) "AcceptsSmallNegativeInts" (Yes :: Formula IntProp)
                ]

--instance HasPlutusTestRunner IntProp Int where
--  expect _ _ = Var IsSmall :&&: Var IsNegative
--  script _ i =
--    let ii = (fromIntegral i) :: Integer
--     in compile (pif (((fromInteger ii) #< ((fromInteger 0) :: Term s PInteger)) #&& (((fromInteger (-10)) :: Term s PInteger) #<= (fromInteger ii))) (pcon PUnit) perror)
--
--intPairGenPlutarchTests :: TestTree
--intPairGenPlutarchTests = testGroup "Plutarch.AcceptsSmallNegativeInts" $
--  fromGroup <$> [
--    runScriptTestsWhere (Proxy :: Proxy Int) (Proxy :: Proxy IntProp) "AcceptsSmallNegativeInts" Yes
--  ]

intPairGenSelfTests :: TestTree
intPairGenSelfTests = testGroup "(Int,Int) HasPermutationGenerator permutationGeneratorSelfTest" $
  fromGroup <$> permutationGeneratorSelfTest (\(_ :: PermutationEdge IntPairProp (Int,Int)) -> True) baseGen


