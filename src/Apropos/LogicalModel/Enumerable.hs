module Apropos.LogicalModel.Enumerable (
  Enumerable (..),
) where

import Control.Monad (liftM2)
import GHC.Generics

class (Eq p, Ord p) => Enumerable p where
  enumerated :: [p]
  default enumerated :: (Generic p, GenericEnum (Rep p)) => [p]
  enumerated = to <$> genericEnumerated

class GenericEnum p where
  genericEnumerated :: [p a]

instance GenericEnum V1 where
  genericEnumerated = []

instance GenericEnum U1 where
  genericEnumerated = [U1]

instance (GenericEnum a, GenericEnum b) => GenericEnum (a :+: b) where
  genericEnumerated = (L1 <$> genericEnumerated) <> (R1 <$> genericEnumerated)

instance (GenericEnum a, GenericEnum b) => GenericEnum (a :*: b) where
  genericEnumerated = liftM2 (:*:) genericEnumerated genericEnumerated

instance Enumerable c => GenericEnum (K1 i c) where
  genericEnumerated = K1 <$> enumerated

instance GenericEnum f => GenericEnum (M1 i t f) where
  genericEnumerated = M1 <$> genericEnumerated
