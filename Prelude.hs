{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home, prune #-}

-- |
-- Custom Prelude that focuses on applying algebra and category theory to ordinary code.
--
-- In particular, comonads are treated as first-class citizens, profunctors are
-- available by default, and safe functions are preferred by default.
module Prelude
  ( module All,

    -- * Type Synonyms and Type Operators
    N,
    Z,
    type (+),
    type (⊕),
    type (×),
    type (⊗),
    type (.),
    type (∘),
    type (~>),

    -- * Operators and Utility Functions
    (!),
    (!!),
    swapF,
    (&&),
    (∧),
    (||),
    (∨),
    (|>),
    (<|),
    (∘),
    (∘∘),
    (∈),
    (.:),
    allBounded,
    reduce,
    futuHoist,
    hoistM,

    -- * Pretty-Printing
    prettyPrint,
    prettyText,
    prettyText',
    tracePretty,
    tracePrettyM,
    tracePrettyId,

    -- * Newtypes
    Sum (..),
    Product (..),

    -- * Function-Like Classes
    Partial (..),
    Functional (..),
    ($),
    (&),
  )
where

import Control.Arrow as All hiding (first, second)
import Control.Category as All
import Control.Comonad as All
import Control.Comonad.Cofree as All (Cofree)
import Control.Comonad.Trans.Cofree as All (CofreeF (..))
import Control.Monad.Free as All (Free)
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
import Data.Functor.Foldable as All hiding (fold)
import qualified Data.Functor.Product as Functor
import Data.Functor.Rep as All
import qualified Data.Functor.Sum as Functor
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.MonoTraversable as All hiding (headMay, lastMay, maximumMay, minimumMay)
import Data.Pointed as All
import Data.Profunctor as All
import Data.Profunctor.Sieve
import Data.Profunctor.Strong as All
import Data.Semigroup as All hiding (Product (..), Sum (..))
import Data.Semigroupoid as All
import qualified Data.Sequence ()
import Data.Sequences as All (Index, IsSequence, SemiSequence)
import qualified Data.Sequences as Seq
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text as Strict
import qualified Data.Text.IO as Strict.IO
import qualified Data.Text.Lazy.IO as Lazy.IO
import Data.These as All
import qualified GHC.Num
import NumHask.Prelude as All hiding (($), (&), (&&), (.), Alt, Distributive, First (..), Last (..), embed, from, hoist, pack, reduce, to, unpack, yield, (||), Product (..), Sum (..), Rep (..), id, lookup, (!!))
import Text.PrettyPrint.Leijen.Text as All (Pretty (..), char, displayT, displayTStrict, nest, renderPretty, text, textStrict)
import Control.Monad.IO.Class
import qualified Data.List as List
import Control.Monad as All
import Debug.Trace as All

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
type (f ~> g) = forall x. f x -> g x

-- | Infix operator for set membership.
(∈) :: SetContainer set => ContainerKey set -> set -> Bool
(∈) = member

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
{-# INLINE (.:) #-}

infixr 8 .:

-- | Alternative synonym for 'cata', so we can still use 'fold'
reduce :: Recursive t => (Base t a -> a) -> t -> a
reduce = cata
{-# INLINE reduce #-}

-- | A variant of hoist that allows multiple layers of the corecursive structure
-- to be generated from a single layer of the recursive structure.
futuHoist ::
  forall s t.
  (Recursive s, Corecursive t) =>
  -- | change of base functor
  (forall x. Base s x -> Base t (Free (Base t) x)) ->
  s ->
  t
futuHoist = hoistM distFutu
{-# INLINE futuHoist #-}

-- | A version of hoist with a distributive law for the corecursive structure
hoistM ::
  forall s t m.
  (Recursive s, Corecursive t, Monad m) =>
  -- | distributive law for t
  (forall x. m (Base t x) -> Base t (m x)) ->
  -- | change of base functor
  (forall x. Base s x -> Base t (m x)) ->
  s ->
  t
hoistM distMT f = go . pure . f . project
  where
    go = embed . fmap (go . fmap (f . project) . join) . distMT

-- | All values in a 'Bounded' 'Enum'
allBounded :: (Enum a, Bounded a) => [a]
allBounded = [minBound .. maxBound]
{-# INLINE allBounded #-}

-- | Replacement for 'print' with nicer output where possible
prettyPrint :: (Pretty a, MonadIO m) => a -> m ()
prettyPrint = liftIO . Strict.IO.putStrLn . prettyText'
{-# INLINE prettyPrint #-}

-- | Pretty-print a type to (lazy) 'Lazy.Text'
prettyText :: Pretty a => a -> Lazy.Text
prettyText = displayT . renderPretty 0.4 80 . pretty
{-# INLINE prettyText #-}

-- | Pretty-print a type to (strict) 'Strict.Text'
prettyText' :: Pretty a => a -> Strict.Text
prettyText' = displayTStrict . renderPretty 0.4 80 . pretty
{-# INLINE prettyText' #-}

{-# WARNING tracePretty "'tracePretty' remains in code" #-}
tracePretty :: Pretty b => b -> a -> a
tracePretty = trace . Strict.unpack . prettyText'

{-# WARNING tracePrettyM "'tracePrettyM' remains in code" #-}
tracePrettyM :: (Monad m, Pretty a) => a -> m ()
tracePrettyM = traceM . Strict.unpack . prettyText'

{-# WARNING tracePrettyId "'tracePrettyId' remains in code" #-}
tracePrettyId :: Pretty a => a -> a
tracePrettyId a = tracePretty a a

-- | A renaming of 'sequence', for situations where it looks nothing like
-- sequencing
--
-- This basically just swaps two wrappers/functors around
--
-- An alternative to this in some situations is 'distribute', for types in the
-- 'Distributive' class
swapF :: forall t f a. (Traversable t, Applicative f) => t (f a) -> f (t a)
swapF = sequenceA
{-# INLINE swapF #-}

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
    Nothing -> error "Index out of bounds"
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

-- | Allow summing without having to define a full 'Num' instance
newtype Sum a = Sum {getSum :: a}
  deriving stock (Eq, Ord, Show, Read, Generic, Functor)

instance Additive a => Semigroup (Sum a) where
  Sum x <> Sum y = Sum (x + y)

instance Additive a => Monoid (Sum a) where
  mempty = Sum zero

-- | Allow taking products of foldables etc. without a full 'Num' instance
newtype Product a = Product {getProduct :: a}
  deriving stock (Eq, Ord, Show, Read, Functor, Generic)

instance Multiplicative a => Semigroup (Product a) where
  Product x <> Product y = Product (x * y)

instance Multiplicative a => Monoid (Product a) where
  mempty = Product one

-- TODO: there's probably some utilities for laying these out nicely.
instance {-# OVERLAPPING #-} (Ord k, Pretty k, Pretty a) => Pretty (Map k a) where
  pretty m = "{ " <> Map.foldMapWithKey prettyItem m <> "}"
    where
      prettyItem k a = pretty k <> ": " <> pretty a <> " "

instance {-# OVERLAPPING #-} (Ord a, Pretty a) => Pretty (Set a) where
  pretty s = "{" <> foldMap id (List.intersperse (", ") (pretty <$> (Set.toList s))) <> "}"

-- | Make everything with the appropriate methods part of the 'Num' class by default.
instance {-# OVERLAPPABLE #-} (Ring a, Signed a, FromInteger a) => Num a where
  (+) = (+)
  (*) = (*)
  abs = abs
  signum = signum
  fromInteger = fromInteger
  negate = negate
  (-) = (-)
