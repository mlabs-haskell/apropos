module Proper.IsDeviceModel ( IsDeviceModel(..), Device(..) ) where
import Proper.HasProperties
import Proper.HasParameterisedGenerator
import Proper.Proposition
import Hedgehog (Property,Group(..),property,(===))
import SAT.MiniSat ( Formula (..) )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)

data Device m p =
  Device {
    expect :: Formula p
  , run :: m -> Bool
  }

class (HasProperties m p, HasParameterisedGenerator m p) => IsDeviceModel m p where
  runDeviceTest :: Device m p -> Set p -> Property
  runDeviceTest device s = property $ do
    (m :: m) <- parameterisedGenerator s
    satisfiesFormula (expect device) s === (run device m)
  runDeviceTestsWhere :: Device m p -> String -> Formula p -> Group
  runDeviceTestsWhere device name condition =
    Group (fromString name) $
      [ (fromString $ show $ Set.toList scenario, runDeviceTest device scenario)
      | scenario <- enumerateScenariosWhere condition
      ]

