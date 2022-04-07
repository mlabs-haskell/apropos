import Criterion.Main

import Spec.Rational
import Apropos
import Apropos.LogicalModel
import Hedgehog
import System.Environment (setEnv)

main :: IO ()
main = do
  setEnv "HEDGEHOG_VERBOSITY" "0"
  defaultMain
    [ bench "rational sat" $ nf (length . enumerateSolutions) (logic @RatProp)
    , bench "full rational test" $ nfIO
      $ checkParallel $ runGeneratorTestsWhere (Apropos :: Rat :+ RatProp) "generator" Yes
    ]
