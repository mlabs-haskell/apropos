module Apropos.Gen.Cardinality (
  Cardinality(..),
  HasCardinality(..),
  genCardinality,
  ) where
import Apropos.Gen
--import Control.Monad.Free

data Cardinality =
    Contiguous Range
  | Product [Cardinality]
  | Sum [Cardinality]

class HasCardinality t where
  cardinality :: t -> Cardinality
  cardinality _ = Contiguous (singleton 1)


genCardinality :: Gen a -> Cardinality
genCardinality _ = undefined
