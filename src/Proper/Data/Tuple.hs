module Proper.Data.Tuple () where
import Proper.HasProperties
import Proper.Proposition
import Proper.HasParameterisedGenerator
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Proxy (Proxy(..))
import Control.Monad (join)
import SAT.MiniSat ( Formula (..) )

data TupleProp a b =
      LeftProp a
    | RightProp b
    deriving stock (Eq,Ord,Show)

instance (Proposition a, Proposition b) => Bounded (TupleProp a b) where
  minBound = LeftProp minBound
  maxBound = RightProp maxBound

instance (Proposition a, Proposition b) => Enum (TupleProp a b) where
  toEnum i =
    let sizeA = length ([minBound..maxBound] :: [a])
     in if i < sizeA
          then LeftProp (toEnum i)
          else RightProp (toEnum (i - sizeA))
  fromEnum (LeftProp a) = fromEnum a
  fromEnum (RightProp b) =
    let sizeA = length ([minBound..maxBound] :: [a])
     in sizeA + fromEnum b

instance (Proposition a, Proposition b) => Proposition (TupleProp a b) where
  logic = Yes

instance (HasProperties a ap, HasProperties b bp) => HasProperties (a,b) (TupleProp ap bp) where
  satisfiesProperty (a,_) (LeftProp ap) = satisfiesProperty a ap
  satisfiesProperty (_,b) (RightProp bp) = satisfiesProperty b bp

leftProps :: Ord a => Set (TupleProp a b) -> Set a
leftProps s = Set.fromList $ join (leftProp <$> Set.toList s)
  where
    leftProp :: TupleProp a b -> [a]
    leftProp (LeftProp a) = [a]
    leftProp _ = []

rightProps :: Ord b => Set (TupleProp a b) -> Set b
rightProps s = Set.fromList $ join (rightProp <$> Set.toList s)
  where
    rightProp :: TupleProp a b -> [b]
    rightProp (RightProp b) = [b]
    rightProp _ = []

instance ( HasParameterisedGenerator a ap
         , HasParameterisedGenerator b bp
         ) => HasParameterisedGenerator (a,b) (TupleProp ap bp) where
  parameterisedGenerator _ s =
    (,) <$> parameterisedGenerator (Proxy :: Proxy a) (leftProps s)
        <*> parameterisedGenerator (Proxy :: Proxy b) (rightProps s)

