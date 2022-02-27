module Apropos.Gen.Cardinality (
  Cardinality(..),
  HasCardinality(..),
  genCardinality,
  ) where
import Apropos.Gen
import Apropos.Gen.Range
import Control.Monad.Free

data Cardinality =
    Scalar Integer
  | Product [Cardinality]
  | Sum [Cardinality]

class HasCardinality t where
  cardinality :: t -> Cardinality
  cardinality _ = Scalar 1

unit :: Cardinality
unit = Scalar 1

_range :: Range -> Cardinality
_range (Singleton _) = Scalar 1
_range (Linear a b) = Scalar $ fromIntegral $ b - a

genCardinality :: forall a . Gen a -> Cardinality
genCardinality (Free (Label _ next)) = genCardinality next
genCardinality (Free (FailWithFootnote _ _)) = unit
genCardinality _ = undefined
--genCardinality (Free (GenBool next)) = genCardinality (next (Scalar 2))
--genCardinality (Free (GenInt r next)) = 

--the thought here is to compute the size of the codomain of a generator morphism

