module Test.Data.List where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Data.List

instance arbitraryList :: (Arbitrary a) => Arbitrary (List a) where
  arbitrary = fromArray <$> arbitrary
