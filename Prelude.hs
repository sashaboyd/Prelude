{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home, prune #-}

-- |
-- Custom Prelude that focuses on applying algebra and category theory to ordinary code.
--
-- In particular, comonads are treated as first-class citizens, profunctors are available by default, and
module Prelude
  ( module All,
    N,
    (!),
    swapF,
    prettyPrint,
    (&&),
    (||),
    (|>),
    (|>>),
    (<|),
    (<<|),
    (.:),
  )
where

import Control.Arrow as All hiding (first, second)
import Control.Category as All
import Control.Comonad as All
import Control.Comonad.Cofree as All (Cofree)
import Control.Comonad.Trans.Cofree as All (CofreeF (..))
import Control.Monad.Free as All (Free)
import Data.Coerce as All
import Data.Distributive as All
import Data.Functor.Classes as All
import Data.Functor.Compose as All
import Data.Functor.Foldable as All
import Data.Functor.Rep as All
import Data.Profunctor as All
import Data.Profunctor.Strong as All
import Data.Semigroup as All
import Data.These as All
import GHC.Natural (intToNatural)
import NumHask.Prelude as All hiding ((&&), (.), Distributive, First (..), Last (..), fold, yield, (||))
import Text.PrettyPrint.Leijen.Text as All (Pretty (..), renderPretty, text, char, textStrict, nest)

-- | Shorthand for natural numbers
type N = Natural

-- | Shorthand for integers
type Z = Integer

-- | Use 'index' from the 'Representable' class for indexing by default
(!) :: forall f a. Representable f => f a -> Rep f -> a
(!) = index

infixl 9 !

-- | Conjunction that works with more than just 'Bool's
(&&) :: forall a. MeetSemiLattice a => a -> a -> a
(&&) = (/\)

infixr 3 &&

-- | Disjunction that works with more than just 'Bool's
(||) :: forall a. JoinSemiLattice a => a -> a -> a
(||) = (\/)

infixr 2 ||

-- | A “pipe” for mapping a function over the codomain of a profunctor
--
-- This can also be used just for function composition, similar to '.'
(<|) :: Profunctor p => (b -> c) -> p a b -> p a c
(<|) = rmap

infixr 9 <|

-- | A “pipe” for a curried profunctor with two arguments
(<<|) ::
  (Profunctor p1, Profunctor p2) =>
  (b -> c) ->
  p1 a1 (p2 a2 b) ->
  p1 a1 (p2 a2 c)
(<<|) = rmap rmap rmap

infixr 8 <<|

-- | A “pipe” for mapping over the domain of a of a profunctor
--
-- This can also be used just for function composition, similar to '>>>'
(|>) :: Profunctor p => (a -> b) -> p b c -> p a c
(|>) = lmap

infixr 9 |>

-- | A “pipe” for mapping over the domain of the domain of a profunctor
(|>>) ::
  (Profunctor p1, Profunctor p2) =>
  (a -> b) ->
  p2 (p1 a c1) c2 ->
  p2 (p1 b c1) c2
(|>>) = lmap lmap lmap

infixr 8 |>>

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

-- | Replacement for 'print' with nicer output where possible
prettyPrint :: (Pretty a, MonadIO m) => a -> m ()
prettyPrint = print . renderPretty 0.4 80 . pretty

-- | A renaming of 'sequence', for situations where it looks nothing like
-- sequencing
--
-- This basically just swaps two wrappers/functors around
swapF :: forall t f a. (Traversable t, Applicative f) => t (f a) -> f (t a)
swapF = sequenceA

-- | 'Natural's don't have a 'Normed' instance for some reason, so we define it here
instance Normed Int Natural where
  normL1 = intToNatural . abs
  normL2 = intToNatural . abs

-- | Rectilinear norm for 'Int' 'NumHask.Data.Pair's
--
-- Only the L¹ norm is defined, since the L² norm doesn't make sense here
--
-- NOTE: Because of the way variables are matched and then constrained, Pair Int
-- technically matches the (ExpField a, Normed a a) => Normed (Pair a) a
-- instance, even though Ints don't match the ExpField constraint, so we have to
-- declare this instance as overlapping that one.
instance {-# OVERLAPPING #-} Normed (Pair Int) Int where
  normL1 (Pair x y) = normL1 x + normL1 y
  normL2 (Pair _ _) = throw (NoMethodError "normL2 can't be defined for Int pairs")

-- | Rectilinear norm for 'Int' 'NumHask.Data.Pair's that returns a 'Natural'
instance {-# OVERLAPPING #-} Normed (Pair Int) Natural where
  normL1 (Pair x y) = normL1 x + normL1 y
  normL2 (Pair _ _) = throw (NoMethodError "normL2 can't be defined for Int pairs")

-- | Rectilinear distance for 'Natural' 'NumHask.Data.Pair's
instance {-# OVERLAPPING #-} Normed (Pair Natural) Natural where
  normL1 (Pair x y) = normL1 x + normL1 y
  normL2 (Pair _ _) = throw (NoMethodError "normL2 can't be defined for Int pairs")

-- | Rectilinear distance for 'Int' 'NumHask.Data.Pair's
--
-- This goes by a bunch of different names, like Manhattan distance, taxicab
-- distance, etc.
--
-- Only the L¹ distance is defined, since L² distance doesn't make sense here
instance {-# OVERLAPPING #-} Metric (Pair Int) Int where
  distanceL1 a b = normL1 (a - b)
  distanceL2 _ _ = throw (NoMethodError "distanceL2 can't be defined for Int pairs")

-- | Rectilinear distance that returns a 'Natural'
instance {-# OVERLAPPING #-} Metric (Pair Int) Natural where
  distanceL1 a b = normL1 (a - b)
  distanceL2 _ _ = throw (NoMethodError "distanceL2 can't be defined for Int pairs")

-- | Rectilinear distance for 'Natural' 'NumHask.Data.Pair's
instance {-# OVERLAPPING #-} Metric (Pair Natural) Natural where
  distanceL1 a b = normL1 (a - b)
  distanceL2 _ _ = throw (NoMethodError "distanceL2 can't be defined for Int pairs")
