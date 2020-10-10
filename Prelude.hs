{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home, prune #-}

-- |
-- Custom Prelude that focuses on applying algebra and category theory to ordinary code.
--
-- In particular, comonads are treated as first-class citizens, profunctors are
-- available by default, and safe functions are preferred by default.
module Prelude
  ( module All,
    N,
    Z,
    type (+),
    type (⊕),
    type (×),
    type (⊗),
    type (.),
    type (∘),
    type (~>),
    (!),
    (!!),
    swapF,
    prettyPrint,
    prettyText,
    prettyText',
    (&&),
    (∧),
    (||),
    (∨),
    (|>),
    (<|),
    (∘),
    (∘∘),
    (.:),
    goWith,
    getOut,
    Partial (..),
    Functional (..),
    ($),
    (&),
    allBounded,
  )
where

import Control.Arrow as All hiding (first, second)
import Control.Category as All
import Control.Comonad as All
import Control.Comonad.Cofree as All (Cofree)
import Control.Comonad.Trans.Cofree as All (CofreeF (..))
import Control.Monad.Free as All (Free)
import Control.Newtype as All
import Data.Bifunctor.Apply as All
import Data.Coerce as All
import Data.Containers as All
import Data.Copointed as All
import Data.Default.Class as All
import Data.Distributive as All
import Data.Functor.Apply as All
import Data.Functor.Classes as All
import Data.Functor.Compose as All
import Data.Functor.Extend as All
import Data.Functor.Foldable as All
import qualified Data.Functor.Product as Functor
import Data.Functor.Rep as All
import qualified Data.Functor.Sum as Functor
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.MonoTraversable as All hiding (headMay, lastMay, maximumMay, minimumMay)
import Data.Pointed as All
import Data.Profunctor as All
import Data.Profunctor.Sieve
import Data.Profunctor.Strong as All
import Data.Semigroup as All
import Data.Semigroupoid as All
import qualified Data.Sequence ()
import Data.Sequences as All (Index, IsSequence, SemiSequence)
import qualified Data.Sequences as Seq
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Lazy
import Data.These as All
import GHC.Natural (intToNatural, naturalFromInteger)
import qualified GHC.Num
import NumHask.Prelude as All hiding (($), (&), (&&), (.), Alt, Distributive, First (..), Last (..), embed, fold, from, hoist, pack, to, unpack, yield, (||))
import Text.PrettyPrint.Leijen.Text as All (Pretty (..), char, displayT, displayTStrict, nest, renderPretty, text, textStrict)

-- | Shorthand for natural numbers
type N = Natural

-- | Shorthand for integers
type Z = Integer

-- | Infix version of sum types
type a + b = Either a b

-- | Infix version of the sum of two functors
type f ⊕ g = Functor.Sum f g

-- | Unicode product type
type a × b = (a, b)

-- | Infix version of the product of two functors
type f ⊗ g = Functor.Product f g

-- | Infix operator for functor composition
type f ∘ g = Compose f g

-- | Infix operator for functor composition
type f . g = Compose f g

-- | Natural transformation between two functors.
type (f ~> g) a = f a -> g a

-- | Conjunction that works with more than just 'Bool's
(&&) :: forall a. MeetSemiLattice a => a -> a -> a
(&&) = (/\)

infixr 3 &&

(∧) :: forall a. MeetSemiLattice a => a -> a -> a
(∧) = (/\)

infixr 3 ∧

-- | Disjunction that works with more than just 'Bool's
(||) :: forall a. JoinSemiLattice a => a -> a -> a
(||) = (\/)

infixr 2 ||

(∨) :: forall a. JoinSemiLattice a => a -> a -> a
(∨) = (\/)

(∘) ::
  forall k a b c.
  Category k =>
  b `k` c ->
  (a `k` b) ->
  (a `k` c)
(∘) = (.)

-- | Composition with a function (or more generally, morphism) of two arguments
--
-- > (f ∘∘ g) x y = f (g x y)
--
-- NOTE: this is generalized to categories, but you can read the type signature as:
--
-- > (∘∘) :: (b -> c) -> (a1 -> a2 -> b) -> (a1 -> a2 -> c)
(∘∘) ::
  forall k a1 a2 b c.
  Category k =>
  b `k` c ->
  (a1 -> (a2 `k` b)) ->
  a1 ->
  (a2 `k` c)
(∘∘) = (∘) (∘) (∘)

-- | Composition with a function (or more generally, morphism) of two arguments
--
-- > (f .: g) x y = f (g x y)
--
-- NOTE: this is generalized to categories, but you can read the type signature as:
--
-- > (.:) :: (b -> c) -> (a1 -> a2 -> b) -> (a1 -> a2 -> c)
(.:) ::
  forall k a1 a2 b c.
  Category k =>
  b `k` c ->
  (a1 -> (a2 `k` b)) ->
  a1 ->
  (a2 `k` c)
(.:) = (.) (.) (.)

infixr 8 .:

-- | All values in a 'Bounded' 'Enum'
allBounded :: (Enum a, Bounded a) => [a]
allBounded = [minBound .. maxBound]

-- | More readable synonym for 'point'
goWith :: forall p a. Pointed p => a -> p a
goWith = point

-- | More readable synonym for 'copoint'
getOut :: forall p a. Copointed p => p a -> a
getOut = copoint

-- | Replacement for 'print' with nicer output where possible
prettyPrint :: (Pretty a, MonadIO m) => a -> m ()
prettyPrint = putStrLn . prettyText

-- | Pretty-print a type to (lazy) 'Lazy.Text'
prettyText :: Pretty a => a -> Lazy.Text
prettyText = displayT . renderPretty 0.4 80 . pretty

-- | Pretty-print a type to (strict) 'Text'
prettyText' :: Pretty a => a -> Text
prettyText' = displayTStrict . renderPretty 0.4 80 . pretty

-- | A renaming of 'sequence', for situations where it looks nothing like
-- sequencing
--
-- This basically just swaps two wrappers/functors around
--
-- An alternative to this in some situations is 'distribute', for types in the
-- 'Distributive' class
swapF :: forall t f a. (Traversable t, Applicative f) => t (f a) -> f (t a)
swapF = sequenceA

-- | A 'Map' as a 'Profunctor'
--
-- The ‘domain’ of the profunctor represents a value that might be turned into a
-- key, while the codomain represents a value that might belong to the map
newtype MapProf k a b = MapProf'
  { getMapProf :: (a -> Maybe k, Map k b)
  }
  deriving (Generic, Functor)

pattern MapProf :: (a -> Maybe k) -> Map k b -> MapProf k a b
pattern MapProf f m = MapProf' (f, m)

{-# COMPLETE MapProf #-}

instance Profunctor (MapProf k) where
  lmap f (MapProf g m) = MapProf (lmap f g) m
  rmap f (MapProf g m) = MapProf g (f <$> m)

-- | Partial functions and dictionary-like objects
class Partial a b p | p -> a, p -> b where
  runPartial :: p -> a -> Maybe b
  unsafeRunPartial :: p -> a -> b
  unsafeRunPartial p a = case runPartial p a of
    Nothing -> panic "Index out of bounds"
    Just b -> b

instance Partial a b (a -> Maybe b) where runPartial = id

instance Ord k => Partial k a (Map k a) where runPartial = flip Map.lookup

instance Partial Int a (IntMap a) where runPartial = flip IntMap.lookup

instance {-# OVERLAPPABLE #-} (IsSequence s, i ~ Index s, e ~ Element s) => Partial i e s where runPartial = Seq.index

-- | Things that can be ‘run’ or ‘applied’ to some argument
class Functional a b r | r -> a, r -> b where
  run :: r -> a -> b

instance Functional a b (a -> b) where
  run f a = f a

-- NOTE: unfortunately, this instance conflicts with the cosieve instance
-- instance {-# OVERLAPPABLE #-} Sieve (p f) f => Functional a (f b) (p f a b) where run = sieve

instance {-# OVERLAPPABLE #-} Cosieve (p f) f => Functional (f a) b (p f a b) where run = cosieve

instance {-# OVERLAPPABLE #-} (Representable f, Rep f ~ a) => Functional a b (f b) where run = index

-- | Infix synonym for 'runPartial'; allows indexing into dictionary-like
-- objects and running partial functions
(!) :: forall a b p. Partial a b p => p -> a -> Maybe b
(!) = runPartial

infixl 9 !

-- | Infix synonym for 'unsafeRunPartial'
(!!) :: forall a b p. Partial a b p => p -> a -> b
(!!) = unsafeRunPartial

-- | Generalized version of 'Prelude.$' from base
($) :: forall a b r. Functional a b r => r -> a -> b
($) = run

infixr 0 $

(&) :: forall a b r. Functional a b r => a -> r -> b
(&) = flip run

infixl 1 &

-- | Infix version of 'run'
--
-- You can think of this as like a pipe with an arrow pointing in the direction
-- of the flow of data.
(<|) :: forall a b r. Functional a b r => r -> a -> b
(<|) = run

infixr 1 <|

-- | Infix version of 'run', but with the input on the left
--
-- You can think of this as like a pipe with an arrow pointing in the direction
-- of the flow of data.
(|>) :: forall a b r. Functional a b r => a -> r -> b
(|>) = flip run

infixl 1 |>

-- | 'Normed' instance for 'Int's that returns a 'Natural'
instance Normed Int Natural where
  norm = intToNatural . abs

-- | 'Normed' instance for 'Integer's that returns a 'Natural'
instance Normed Integer Natural where
  norm = naturalFromInteger . abs

-- | Rectilinear norm for 'Int' 'NumHask.Data.Pair's
--
-- NOTE: Because of the way variables are matched and then constrained, Pair Int
-- technically matches the (ExpField a, Normed a a) => Normed (Pair a) a
-- instance, even though Ints don't match the ExpField constraint, so we have to
-- declare this instance as overlapping that one.
instance {-# OVERLAPPING #-} Normed (Pair Int) Int where
  norm (Pair x y) = norm x + norm y

instance {-# OVERLAPPING #-} Normed (Pair Integer) Integer where
  norm (Pair x y) = norm x + norm y

-- | Rectilinear norm for 'Int' 'NumHask.Data.Pair's that returns a 'Natural'
instance {-# OVERLAPPING #-} Normed (Pair Int) Natural where
  norm (Pair x y) = norm x + norm y

instance {-# OVERLAPPING #-} Normed (Pair Integer) Natural where
  norm (Pair x y) = norm x + norm y

-- | Rectilinear distance for 'Natural' 'NumHask.Data.Pair's
instance {-# OVERLAPPING #-} Normed (Pair Natural) Natural where
  norm (Pair x y) = norm x + norm y

-- | Rectilinear distance for 'Int' 'NumHask.Data.Pair's
--
-- This goes by a bunch of different names, like Manhattan distance, taxicab
-- distance, etc.
instance {-# OVERLAPPING #-} Metric (Pair Int) Int where
  distance a b = norm (a - b)

instance {-# OVERLAPPING #-} Metric (Pair Integer) Integer where
  distance a b = norm (a - b)

-- | Rectilinear distance that returns a 'Natural'
instance {-# OVERLAPPING #-} Metric (Pair Int) Natural where
  distance a b = norm (a - b)

instance {-# OVERLAPPING #-} Metric (Pair Integer) Natural where
  distance a b = norm (a - b)

-- | Rectilinear distance for 'Natural' 'NumHask.Data.Pair's
instance {-# OVERLAPPING #-} Metric (Pair Natural) Natural where
  distance a b = norm (a - b)

-- TODO: there's probably some utilities for laying these out nicely.
instance (Ord k, Pretty k, Pretty a) => Pretty (Map k a) where
  pretty m = "{ " <> Map.foldMapWithKey prettyItem m <> "}"
    where
      prettyItem k a = pretty k <> ": " <> pretty a <> " "

instance (Ord a, Pretty a) => Pretty (Set a) where
  pretty s = "{" <> foldMap id (intersperse (", ") (pretty <$> (Set.toList s))) <> "}"

-- | Make everything with the appropriate methods part of the 'Num' class by default.
instance {-# OVERLAPPABLE #-} (Ring a, Signed a, FromInteger a) => Num a where
  (+) = (+)
  (*) = (*)
  abs = abs
  signum = signum
  fromInteger = fromInteger
  negate = negate
  (-) = (-)
