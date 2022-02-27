module Apropos.HasPermutationGenerator (
  HasPermutationGenerator(..),
  PermutationEdge(..),
  Abstraction(..),
  abstract,
  abstractsProperties,
  (|:->),
  ) where
import Apropos.Type
import Apropos.Gen
import Apropos.HasLogicalModel
import Apropos.LogicalModel
import Apropos.HasPermutationGenerator.Contract
import Apropos.HasPermutationGenerator.PermutationEdge
import Apropos.HasPermutationGenerator.Abstraction
import Data.Set (Set)
import qualified Data.Set as Set
import Hedgehog (Group(..),failure,property)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Graph (Graph)
import Data.Graph (buildG,scc,path)
import Text.Show.Pretty (ppDoc)
import Text.PrettyPrint (
  Style (lineLength),
  hang,
  renderStyle,
  style,
  ($+$),
 )
import Control.Monad (join)
import Data.String (fromString)

class (HasLogicalModel p m, Show m) => HasPermutationGenerator p m where

  generators :: [PermutationEdge p m]

  permutationGeneratorSelfTest :: Bool -> (PermutationEdge p m -> Bool) -> Gen m m -> [Group]
  permutationGeneratorSelfTest testForSuperfluousEdges pefilter bgen =
    let pedges = findPermutationEdges (Apropos :: m :+ p)
        (_,ns) = numberNodes (Apropos :: m :+ p)
        mGen = buildGen bgen
        graph = buildGraph pedges
        isco = isStronglyConnected graph
     in if length (Map.keys pedges) == 0
          then [Group "No permutation edges defined."
                [(fromString "no edges defined"
                 ,genProp $ failWithFootnote "no PermutationEdges defined"
                 )]]
          else if isco
                 then case findDupEdgeNames of
                        [] -> testEdge testForSuperfluousEdges ns pedges mGen
                                 <$> filter pefilter generators
                        dups -> [Group "HasPermutationGenerator edge names must be unique." $
                                 [(fromString $ dup <> " not unique", property $ failure)
                                 | dup <- dups]
                                ]
                 else [Group "HasPermutationGenerator Graph Not Strongly Connected" $
                                [(fromString "Not strongly connected", abortNotSCC ns graph)]
                              ]
    where
      abortNotSCC ns graph =
        let (a,b) = findNoPath (Apropos :: m :+ p) ns graph
          in genProp $ failWithFootnote $ renderStyle ourStyle $
               "PermutationEdges do not form a strongly connected graph."
               $+$ hang "No Edge Between here:" 4 (ppDoc a)
               $+$ hang "            and here:" 4 (ppDoc b)
      findDupEdgeNames = [ name g | g <- generators :: [PermutationEdge p m]
                                  , length (filter (==g) generators) > 1 ]
      testEdge :: Bool
               -> Map Int (Set p)
               -> Map (Int,Int) [PermutationEdge p m]
               -> (Set p -> Gen m m)
               -> PermutationEdge p m
               -> Group
      testEdge testRequired ns pem mGen pe =
        Group (fromString (name pe)) $ addRequiredTest testRequired
          [ (edgeTestName f t, runEdgeTest f t)
          | (f,t) <- matchesEdges
          ]
        where
          addRequiredTest False l = l
          addRequiredTest True l = (fromString "Is Required", runRequiredTest):l
          matchesEdges = [ e | (e,v) <- Map.toList pem, pe `elem` v ]
          edgeTestName f t = fromString $ name pe <> " : " <> (show $ Set.toList (lut ns f)) <> " -> " <> (show $ Set.toList (lut ns t))
          isRequired =
            let inEdges = [ length v | (_,v) <- Map.toList pem, pe `elem` v ]
             in any (==1) inEdges
          runRequiredTest = genProp $ do
            if isRequired
               then pure ()
               else failWithFootnote $ renderStyle ourStyle $
                      (fromString $ "PermutationEdge " <> name pe <> " is not required to make graph strongly connected.")
                      $+$ hang "Edge:" 4 (ppDoc $ name pe)
          runEdgeTest f t = genProp $ do
            om <- mGen (lut ns f)
            nm <- liftEdge (permuteGen pe) om
            let expected = lut ns t
                observed = properties nm
            if expected == observed
              then pure ()
              else edgeFailsContract pe om nm expected observed

  buildGen :: Gen m m -> Set p -> Gen m m
  buildGen g = do
    let pedges = findPermutationEdges (Apropos :: m :+ p)
        edges = Map.keys pedges
        distmap = distanceMap edges
        (sn,ns) = numberNodes (Apropos :: m :+ p)
        graph = buildGraph pedges
        isco = isStronglyConnected graph
        go targetProperties = do
          m <- g
          if length pedges == 0
             then failWithFootnote "no PermutationEdges defined"
             else pure ()
          if isco
             then pure ()
             else
               let (a,b) = findNoPath (Apropos :: m :+ p) ns graph
                in failWithFootnote $ renderStyle ourStyle $
                      "PermutationEdges do not form a strongly connected graph."
                      $+$ hang "No Edge Between here:" 4 (ppDoc a)
                      $+$ hang "            and here:" 4 (ppDoc b)
          transformModel sn pedges edges distmap m targetProperties
       in go

  findNoPath :: m :+ p
             -> Map Int (Set p)
             -> Graph
             -> (Set p, Set p)
  findNoPath _ m g = head [ (lut m a, lut m b)
                          | a <- Map.keys m
                          , b <- Map.keys m
                          , not (path g a b)
                          ]

  transformModel :: Map (Set p) Int
                 -> Map (Int,Int) [PermutationEdge p m]
                 -> [(Int,Int)]
                 -> Map Int (Map Int Int)
                 -> m
                 -> Set p
                 -> Gen m m
  transformModel nodes pedges edges distmap m to = do
    pathOptions <- findPathOptions (Apropos :: m :+ p) edges distmap nodes (properties m) to
    traversePath pedges pathOptions m

  traversePath :: Map (Int,Int) [PermutationEdge p m]
                 -> [(Int,Int)] -> m -> Gen m m
  traversePath _ [] m = pure m
  traversePath edges (h:r) m = do
    pe <- case Map.lookup h edges of
            Nothing -> failWithFootnote "this should never happen"
            Just so -> pure so
    tr <- element pe
    let inprops = properties m
        mexpected = runContract (contract tr) (name tr) inprops
    case mexpected of
      Left e -> failWithFootnote e
      Right Nothing -> failWithFootnote $ renderStyle ourStyle $
                    "PermutationEdge doesn't work. This is a model error"
                    $+$ "This should never happen at this point in the program."
      Right (Just expected) -> do
        if satisfiesFormula logic expected
           then pure ()
           else failWithFootnote $ renderStyle ourStyle $
                  "PermutationEdge contract produces invalid model"
                  $+$ hang "Edge:" 4 (ppDoc $ name tr)
                  $+$ hang "Input:" 4 (ppDoc inprops)
                  $+$ hang "Output:" 4 (ppDoc expected)
        label $ fromString $ name tr
        nm <- liftEdge (permuteGen tr) m
        let observed = properties nm
        if expected == observed
          then pure ()
          else edgeFailsContract tr m nm expected observed
        traversePath edges r nm

  findPathOptions :: m :+ p
                  -> [(Int,Int)]
                  -> Map Int (Map Int Int)
                  -> Map (Set p) Int
                  -> Set p -> Set p -> Gen m [(Int,Int)]
  findPathOptions _ edges distmap ns from to = do
    fn <- case Map.lookup from ns of
            Nothing -> failWithFootnote $ renderStyle ourStyle $
                        "Model logic inconsistency found."
                         $+$ hang "A model was found that satisfies these properties:" 4 (ppDoc from)
            Just so -> pure so
    tn <- case Map.lookup to ns of
            Nothing -> failWithFootnote "to node not found"
            Just so -> pure so
    rpath <- genRandomPath edges distmap fn tn
    pure $ pairPath rpath

  buildGraph :: Map (Int,Int) [PermutationEdge p m] -> Graph
  buildGraph pedges =
    let edges = Map.keys pedges
        ub = max (maximum (fst <$> edges)) (maximum (snd <$> edges))
     in buildG (0,ub) edges

  mapsBetween :: Map Int (Set p) -> Int -> Int -> PermutationEdge p m -> Bool
  mapsBetween m a b pedge =
    case runContract (contract pedge) (name pedge) (lut m a) of
      Left e -> error e
      Right Nothing -> False
      Right (Just so) -> satisfiesFormula (match pedge) (lut m a) && so == (lut m b)


  findPermutationEdges :: m :+ p
                       -> Map (Int,Int) [PermutationEdge p m]
  findPermutationEdges apropos =
    let nodemap = snd $ numberNodes apropos
        nodes = Map.keys nodemap
     in Map.fromList [ ((a,b), options )
                     | a <- nodes
                     , b <- nodes
                     , let options = filter (mapsBetween nodemap a b) generators
                     , length options > 0 ]
  numberNodes :: m :+ p
              -> (Map (Set p) Int, Map Int (Set p))
  numberNodes _ =
    let scenarios = enumerateScenariosWhere (logic :: Formula p)
        scennums = Map.fromList $ zip scenarios [0..]
        numsscen = Map.fromList $ zip [0..] scenarios
    in (scennums,numsscen)

pairPath :: [Int] -> [(Int,Int)]
pairPath [] = []
pairPath [_] = []
pairPath (a:b:r) = (a,b):(pairPath (b:r))

isStronglyConnected :: Graph -> Bool
isStronglyConnected g = 1 == length (scc g)

lut :: Show a => Show b => Ord a => Map a b -> a -> b
lut m i = case Map.lookup i m of
           Nothing -> error $ "Not found: " <> show i  <> " in " <> show m <> "\nthis should never happen..."
           Just so -> so

ourStyle :: Style
ourStyle = style {lineLength = 80}

genRandomPath :: [(Int,Int)] -> Map Int (Map Int Int) -> Int -> Int -> Gen m [Int]
genRandomPath edges m from to = go [] from
  where
    go breadcrumbs f =
      let shopasto = lut m f
          shopa = lut shopasto to
          awayfrom = snd <$> filter ((==f) . fst) edges
          diston = (\af -> (af,lut (lut m af) to)) <$> awayfrom
          options = fst <$> filter ((<=shopa) . snd) diston
          options' = filter (\o -> not (o `elem` breadcrumbs)) options
          options'' = case options' of
                        [] -> options
                        _ -> options'
      in case shopa of
           0 -> pure []
           1 -> pure [f,to]
           _ -> do
              p <- element options''
              (f:) <$> go (p:breadcrumbs) p

-- TODO is this a performance bottleneck?
distanceMap :: [(Int,Int)] -> Map Int (Map Int Int)
distanceMap edges =
  let initial = foldr ($) Map.empty (insertEdge <$> edges)
      nodes = Map.keys initial
      algo = distanceMapUpdate <$> nodes
   in go (foldr ($) initial algo) algo
  where
    go m algo =
      if distanceMapComplete m
         then m
         else foldr ($) m algo
    insertEdge :: (Int,Int) -> Map Int (Map Int Int) -> Map Int (Map Int Int)
    insertEdge (f,t) m =
      case Map.lookup f m of
        Nothing -> Map.insert f (Map.fromList [(f,0),(t,1)]) m
        Just so -> Map.insert f (Map.insert t 1 so) m
    distanceMapComplete :: Map Int (Map Int Int) -> Bool
    distanceMapComplete m =
      let nodes = Map.keys m
       in not $ any (> length nodes) $ join [ snd <$> Map.toList (lut m node) | node <- nodes ]
    distanceMapUpdate :: Int -> Map Int (Map Int Int) -> Map Int (Map Int Int)
    distanceMapUpdate node m =
      let nodes = Map.keys m
          know = Map.toList $ lut m node
          unknown = filter (not . (`elem` (fst <$> know))) $ Map.keys m
          news = join $ [ (\(t,d) -> (t,d+dist)) <$> Map.toList (lut m known)
                        | (known,dist) <- (know <> zip unknown (cycle [length nodes + 1]))
                        ]
       in foldr updateDistance m news
      where updateDistance :: (Int,Int) -> Map Int (Map Int Int) -> Map Int (Map Int Int)
            updateDistance (t,d) ma =
              let curdists = lut ma node
               in case Map.lookup t curdists of
                    Nothing -> Map.insert node (Map.insert t d curdists) ma
                    Just d' | d < d' -> Map.insert node (Map.insert t d curdists) ma
                    _ -> ma

edgeFailsContract :: forall m p .
                     HasLogicalModel p m
                  => Show m
                  => PermutationEdge p m -> m -> m -> Set p -> Set p -> Gen m ()
edgeFailsContract tr m nm expected observed =
  failWithFootnote $ renderStyle ourStyle $
    "PermutationEdge fails its contract."
          $+$ hang "Edge:" 4 (ppDoc $ name tr)
          $+$ hang "InputModel:" 4 (ppDoc (ppDoc m))
          $+$ hang "InputProperties" 4 (ppDoc $ Set.toList (properties m :: Set p))
          $+$ hang "OutputModel:" 4 (ppDoc (ppDoc nm))
          $+$ hang "ExpectedProperties:" 4 (ppDoc (Set.toList expected))
          $+$ hang "ObservedProperties:" 4 (ppDoc (Set.toList observed))

