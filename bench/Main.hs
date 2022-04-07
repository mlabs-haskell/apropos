import Criterion.Main

import Spec.Rational
import Apropos
import Apropos.LogicalModel
import Hedgehog
import System.Environment (setEnv)
import Data.DiGraph ( shortestPathCache,shortestPath_ )
import Data.Maybe

main :: IO ()
main = do
  setEnv "HEDGEHOG_VERBOSITY" "0"
  defaultMain
    [ bench "rational sat" $ nf (length . enumerateSolutions) (logic @RatProp)
    , bench "make cache" $ whnf (shortestPathCache . buildGraph) (findMorphisms (Apropos :: Rat :+ RatProp))
    , bench "find paths" $ whnf (\c -> length . concat . catMaybes $ [ shortestPath_ f t c | f <- scenarios @RatProp , t <- scenarios ]) (shortestPathCache (buildGraph (findMorphisms (Apropos :: Rat :+ RatProp))))
    , bench "full rational test" $ nfIO
      $ checkParallel $ runGeneratorTestsWhere (Apropos :: Rat :+ RatProp) "generator" Yes
    ]

