{-# LANGUAGE RankNTypes #-}

module Apropos.HasPermutationGenerator.Morphism (
  Morphism (..),
  (&&&),
  (>>>),
  wrapMorphismWithContractCheck,
) where

import Apropos.Gen
import Apropos.HasLogicalModel
import Apropos.HasPermutationGenerator.Contract
import Apropos.LogicalModel
import Control.Monad ((>=>))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Text.PrettyPrint (
  Style (lineLength),
  hang,
  renderStyle,
  style,
  ($+$),
 )
import Text.Show.Pretty (ppDoc)

data Morphism p m = Morphism
  { name :: String
  , match :: Formula p
  , contract :: Contract p ()
  , morphism :: m -> Gen m
  }

instance Eq (Morphism p m) where
  (==) a b = name a == name b

instance Show (Morphism p m) where
  show = name

(&&&) :: [Morphism p m] -> [Morphism p m] -> [Morphism p m]
(&&&) as bs = [addMorphism a b | a <- as, b <- bs]

(>>>) :: [Morphism p m] -> [Morphism p m] -> [Morphism p m]
(>>>) as bs = [seqMorphism a b | a <- as, b <- bs]

addMorphism :: Morphism p m -> Morphism p m -> Morphism p m
addMorphism a b =
  Morphism
    { -- adding morphisms is analogous to &&&
      name = "(" <> name a <> ") &&& (" <> name b <> ")"
    , match = match a :&&: match b
    , contract = contract a >> contract b
    , morphism = morphism a >=> morphism b
    }

seqMorphism :: Morphism p m -> Morphism p m -> Morphism p m
seqMorphism a b =
  Morphism
    { -- sequencing morphisms is analogous to >>>
      name = "(" <> name a <> ") >>> (" <> name b <> ")"
    , match = match a
    , contract = contract a >> matches (match b) >> contract b
    , morphism = morphism a >=> morphism b
    }

wrapMorphismWithContractCheck :: forall p m. (HasLogicalModel p m, Show m, Enumerable p) => Morphism p m -> Morphism p m
wrapMorphismWithContractCheck mo = mo {morphism = wrap}
  where
    wrap :: m -> Gen m
    wrap m = do
      let inprops = properties m
      nm <- morphism mo m
      let observed = properties nm
      label $ fromString $ name mo
      case runContract (contract mo) inprops of
        Left err ->
          failWithFootnote $
            renderStyle ourStyle $
              "Morphism doesn't work. This is a model error"
                $+$ "This should never happen at this point in the program."
                $+$ ppDoc err
        Right expected ->
          if expected == observed
            then pure nm
            else edgeFailsContract mo m nm expected observed

    edgeFailsContract ::
      Morphism p m ->
      m ->
      m ->
      Set p ->
      Set p ->
      Gen a
    edgeFailsContract tr m nm expected observed =
      failWithFootnote $
        renderStyle ourStyle $
          "Morphism fails its contract."
            $+$ hang "Edge:" 4 (ppDoc $ name tr)
            $+$ hang "InputModel:" 4 (ppDoc (ppDoc m))
            $+$ hang "InputProperties" 4 (ppDoc $ Set.toList (properties m :: Set p))
            $+$ hang "OutputModel:" 4 (ppDoc (ppDoc nm))
            $+$ hang "ExpectedProperties:" 4 (ppDoc (Set.toList expected))
            $+$ hang "ObservedProperties:" 4 (ppDoc (Set.toList observed))

    ourStyle :: Style
    ourStyle = style {lineLength = 80}
