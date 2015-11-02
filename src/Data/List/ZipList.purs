-- | This module defines the type of _zip lists_, i.e. linked lists
-- | with a zippy `Applicative` instance.

module Data.List.ZipList
  ( ZipList(..)
  , runZipList
  ) where

import Prelude

import Control.Alt (Alt)
import Control.Alternative (Alternative)
import Control.Plus (Plus)

import Data.Foldable (Foldable, foldMap, foldl, foldr)
import Data.List.Lazy (List(), repeat, zipWith)
import Data.Monoid (Monoid, mempty)
import Data.Traversable (Traversable, traverse, sequence)

-- | `ZipList` is a newtype around `List` which provides a zippy
-- | `Applicative` instance.
newtype ZipList a = ZipList (List a)

-- | Unpack a `ZipList` to obtain the underlying list.
runZipList :: forall a. ZipList a -> List a
runZipList (ZipList xs) = xs

instance showZipList :: (Show a) => Show (ZipList a) where
  show (ZipList xs) = "(ZipList " ++ show xs ++ ")"

instance eqZipList :: (Eq a) => Eq (ZipList a) where
  eq z1 z2 = runZipList z1 `eq` runZipList z2

instance ordZipList :: (Ord a) => Ord (ZipList a) where
  compare z1 z2 = runZipList z1 `compare` runZipList z2

instance semigroupZipList :: Semigroup (ZipList a) where
  append z1 z2 = ZipList (runZipList z1 ++ runZipList z2)

instance monoidZipList :: Monoid (ZipList a) where
  mempty = ZipList mempty

instance foldableZipList :: Foldable ZipList where
  foldl f b (ZipList xs) = foldl f b xs
  foldr f b (ZipList xs) = foldr f b xs
  foldMap f (ZipList xs) = foldMap f xs

instance traversableZipList :: Traversable ZipList where
  traverse f (ZipList xs) = ZipList <$> traverse f xs
  sequence (ZipList xs) = ZipList <$> sequence xs

instance functorZipList :: Functor ZipList where
  map f (ZipList xs) = ZipList (map f xs)

instance applyZipList :: Apply ZipList where
  apply (ZipList fs) (ZipList xs) = ZipList (zipWith ($) fs xs)

instance applicativeZipList :: Applicative ZipList where
  pure = ZipList <<< repeat

instance altZipList :: Alt ZipList where
  alt = append

instance plusZipList :: Plus ZipList where
  empty = mempty

instance alternativeZipList :: Alternative ZipList
