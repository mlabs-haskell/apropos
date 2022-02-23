{-# LANGUAGE RankNTypes #-}
module Spec.Plutarch.MagicNumber (
  magicNumberPropGenTests
  ) where
import Brutus.HasLogicalModel
import Brutus.LogicalModel
import Brutus.HasParameterisedGenerator
import Brutus.Gen

import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (linear)

import qualified Data.Set as Set

import Plutus.V1.Ledger.Api (Data(..))
import Plutus.V1.Ledger.Scripts (Script, applyArguments, evaluateScript)

import Plutarch (compile)
import Plutarch.Prelude


import Data.Proxy (Proxy(..))
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)

numMagicNumbers :: Integer
numMagicNumbers = 4

runMagicNumber :: Script -> Integer -> Bool
runMagicNumber b i =
    case evaluateScript (applyArguments b [I i]) of
          Right _ -> case evaluateScript (applyArguments b [I (i+1)]) of
                   Left _ -> True
                   _ -> False
          _ -> False

magicNumber :: Integer -> Script
magicNumber i = compile $ plam $ \ii -> (pif (((pfromData ii) #<= ((fromInteger i) :: Term s PInteger)) #&& (((fromInteger (-i)) :: Term s PInteger) #<= (pfromData ii))) (pcon PUnit) perror)


data MagicNumberProp = HalfWidth Integer
              | OutOfRange
  deriving stock (Eq,Ord,Show)

instance Enumerable MagicNumberProp where
  enumerated = OutOfRange:[ HalfWidth hw | hw <- [0..numMagicNumbers] ]

instance LogicalModel MagicNumberProp where
  logic = (ExactlyOne $ Var <$> enumerated)
     :&&: (Some $ Var <$> enumerated)

-- running the scripts in satisfiesProperty is possible
-- we could also have used Script equality here
instance HasLogicalModel MagicNumberProp Script where
  satisfiesProperty (HalfWidth hw) p = runMagicNumber p hw
  satisfiesProperty OutOfRange p = all not [ runMagicNumber p i | i <- [0..numMagicNumbers] ]

-- Now we can generate scripts satsfying some property.
-- It would be more interesting if these were composable terms
-- then we could generate compositions of them in a parent model.
instance HasParameterisedGenerator MagicNumberProp Script where
  parameterisedGenerator s =
    case Set.toList s of
      [HalfWidth i] -> pure $ magicNumber i
      [OutOfRange] -> do
        let outOfMagicNumberRange = [linear minBound (-1)
                             ,linear ((fromInteger numMagicNumbers) + 1) maxBound]
            outOfMagicNumberGen = Gen.choice (Gen.int <$> outOfMagicNumberRange)
        freq <- liftGenP outOfMagicNumberGen
        pure $ magicNumber $ fromIntegral freq
      _ -> error "the impossible happened"

magicNumberPropGenTests :: TestTree
magicNumberPropGenTests = testGroup "Spec.Plutarch.MagicNumber" $
    fromGroup <$> [
      runGeneratorTestsWhere (Proxy :: Proxy Script)
                             "Magic Number Script Generator"
                             (Yes :: Formula MagicNumberProp)
    ]

