{-# LANGUAGE RankNTypes #-}
module Spec.Plutarch.MagicNumber (
  magicNumberPropGenTests
  ) where
import Apropos.HasLogicalModel
import Apropos.LogicalModel
import Apropos.HasParameterisedGenerator

import qualified Data.Set as Set

import Plutus.V1.Ledger.Scripts (Script)

import Plutarch (compile)
import Plutarch.Prelude


import Data.Proxy (Proxy(..))
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)

-- README : This is an interesting thing - this is not investment advice.
-- This makes Script the object of our parameterisedGenerator so we can generate
-- compiled code.
-- It would be more interesting if we could generate composable terms and build
-- higherarchichal models of ASTs. Perhaps that might have some use.
-- This is mostly here as a curiosity - have a think about it...

numMagicNumbers :: Integer
numMagicNumbers = 4

-- accepts a range of numbers determined by a Magic Number
magicNumber :: Integer -> Script
magicNumber i = compile $ plam $ \ii -> (pif (((pfromData ii) #<= ((fromInteger i) :: Term s PInteger)) #&& (((fromInteger (-i)) :: Term s PInteger) #<= (pfromData ii))) (pcon PUnit) perror)

data MagicNumberProp = HalfWidth Integer
  deriving stock (Eq,Ord,Show)

instance Enumerable MagicNumberProp where
  enumerated = [ HalfWidth hw | hw <- [0..numMagicNumbers] ]

instance LogicalModel MagicNumberProp where
  logic = (ExactlyOne $ Var <$> enumerated)

-- running the scripts in satisfiesProperty is possible
instance HasLogicalModel MagicNumberProp Script where
  satisfiesProperty (HalfWidth hw) p = magicNumber hw == p

-- Now we can generate scripts satsfying some property.
-- There is no randomness here but you could throw some in.
-- We should probably have a HasParameterisedEnumerator for this.
-- That would allow us to do exhaustive search of a model.
instance HasParameterisedGenerator MagicNumberProp Script where
  parameterisedGenerator s =
    case Set.toList s of
      [HalfWidth i] -> pure $ magicNumber i
      _ -> error "the impossible happened"

magicNumberPropGenTests :: TestTree
magicNumberPropGenTests = testGroup "Spec.Plutarch.MagicNumber" $
    fromGroup <$> [
      runGeneratorTestsWhere (Proxy :: Proxy Script)
                             "Magic Number Script Generator"
                             (Yes :: Formula MagicNumberProp)
    ]

