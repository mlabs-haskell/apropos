module Proper.IsDeviceModel ( IsDeviceModel(..), Device(..), Result(..) ) where
import Proper.HasProperties
import Proper.HasParameterisedGenerator
import Proper.Proposition
import Hedgehog (Property,Group(..),property,forAll,(===))
import SAT.MiniSat ( Formula (..) )
import Data.Proxy (Proxy(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)

data Device m =
  Device {
    run :: m -> Result
  }

data Result = Pass | Fail deriving stock (Show,Eq)

class (HasProperties m p, HasParameterisedGenerator m p) => IsDeviceModel m p where
  expectedBehaviour :: Proxy m -> Formula p
  runDeviceTest :: Device m -> Set p -> Property
  runDeviceTest device s = property $ do
    m <- forAll $ parameterisedGenerator (Proxy :: Proxy m) s
    if satisfiesFormula (expectedBehaviour (Proxy :: Proxy m)) s
       then (run device) m === Pass
       else (run device) m === Fail
  runDeviceTestsWhere :: Device m -> String -> Formula p -> Group
  runDeviceTestsWhere device name condition =
    Group (fromString name) $
      [ (fromString $ show $ Set.toList scenario, runDeviceTest device scenario)
      | scenario <- enumerateScenariosWhere condition
      ]

