{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Common
  ( module All,
  )
where

import Prelude

import Test.HUnit.Base as All
import Test.Hspec as All
import Test.QuickCheck as All hiding (Testable)
import Test.QuickCheck.Property

-- | Required superclass for the lattice classes, but the operators are trivial
instance Eq Property where
  _ == _ = False
  _ /= _ = True

instance MeetSemiLattice Property where (/\) = (.&&.)

instance JoinSemiLattice Property where (\/) = (.||.)

instance Lattice Property

instance BoundedJoinSemiLattice Property where bottom = property rejected

instance BoundedMeetSemiLattice Property where top = property succeeded

instance BoundedLattice Property

instance Arbitrary Natural where
  arbitrary = fromInteger . abs <$> arbitrary
  shrink = shrinkIntegral

instance CoArbitrary Natural where
  coarbitrary = variant

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary
  shrink (Pair x y) = Pair <$> (shrink x) <*> (shrink y)

instance CoArbitrary a => CoArbitrary (Pair a)

instance Arbitrary2 These where
  liftArbitrary2 gena genb = oneof [This <$> gena, That <$> genb, These <$> gena <*> genb]
  liftShrink2 shrinka _ (This a) = This <$> shrinka a
  liftShrink2 _ shrinkb (That b) = That <$> shrinkb b
  liftShrink2 shrinka shrinkb (These a b) =
    join
      [ This <$> shrinka a,
        That <$> shrinkb b,
        These <$> shrinka a <*> shrinkb b
      ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (These a b) where
  arbitrary = arbitrary2
  shrink = shrink2

instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (These a b)
