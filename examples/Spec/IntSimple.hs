module Spec.IntSimple (
  intSimpleGenTests,
  intSimplePureTests,
  IntDescr,
) where

import Apropos
import Apropos.Description
import Hedgehog (Group, MonadGen, assert)
import Hedgehog.Gen (int)
import Hedgehog.Range (linear)

data IntDescr = IntDescr
  { sign :: Sign
  , size :: Size
  , isBound :: Bool
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (SOPGeneric, HasDatatypeInfo)

data Sign = Positive | Negative | Zero
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (SOPGeneric, HasDatatypeInfo)

data Size = Large | Small
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (SOPGeneric, HasDatatypeInfo)

instance Description IntDescr Int where
  describe i =
    IntDescr
      { sign =
          case compare i 0 of
            GT -> Positive
            EQ -> Zero
            LT -> Negative
      , size =
          if i > 10 || i < -10
            then Large
            else Small
      , isBound = i == minBound || i == maxBound
      }

  additionalLogic =
    All
      [ v [("IntDescr", "sign")] "Zero" :->: v [("IntDescr", "size")] "Small"
      , v [("IntDescr", "isBound")] "True" :->: v [("IntDescr", "size")] "Large"
      ]

  genForDescription s =
    case sign s of
      Zero -> pure 0
      Positive -> intGen
      Negative -> intGen
    where
      bound :: Int
      sig :: Int -> Int
      (bound, sig) =
        case sign s of
          Positive -> (maxBound, id)
          Negative -> (minBound, negate)
          Zero -> (0, id)

      intGen :: (MonadGen m) => m Int
      intGen =
        if isBound s
          then pure bound
          else case size s of
            Small -> int (linear (sig 1) (sig 10))
            Large -> int (linear (sig 11) (bound + sig (-1)))

intSimpleGenTests :: Group
intSimpleGenTests = selfTest @IntDescr

intSimplePureTests :: Group
intSimplePureTests =
  runTests @IntDescr
    "AcceptsSmallNegativeInts" $
    AproposTest
    { expect = \d -> size d == Small && sign d == Negative
    , test   = \i -> assert $ i < 0 && i >= -10
    }
