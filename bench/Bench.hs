import Apropos
import Apropos.LogicalModel (enumerateSolutions)
import Criterion.Main (bench, defaultMain, nf, nfIO, whnf)
import Data.DiGraph (shortestPathCache, shortestPath_)
import Data.Maybe (catMaybes)
import Hedgehog (checkParallel)
import Spec.Rational (Rat, RatProp)
import System.Environment (setEnv)

main :: IO ()
main = do
  setEnv "HEDGEHOG_VERBOSITY" "0"
  defaultMain
    [ bench "rational sat" $ nf (length . enumerateSolutions) (logic @RatProp)
    , bench "make cache" $ whnf (shortestPathCache . buildGraph) (findMorphisms (Apropos :: Rat :+ RatProp))
    , bench "find paths" $ whnf (\c -> length . concat . catMaybes $ [shortestPath_ f t c | f <- scenarios @RatProp, t <- scenarios]) (shortestPathCache (buildGraph (findMorphisms (Apropos :: Rat :+ RatProp))))
    , bench "full rational test" $
        nfIO $
          checkParallel $ runGeneratorTestsWhere (Apropos :: Rat :+ RatProp) "generator" Yes
    ]
