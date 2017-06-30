module Data.List.NonEmpty
  ( module Data.List.Types
  , toUnfoldable
  , fromFoldable
  , fromList
  , toList
  , singleton
  , cons
  , snoc
  , head
  , last
  , tail
  , init
  , uncons
  , unsnoc
  , reverse
  , length
  , concatMap
  , filter
  , filterM
  , mapMaybe
  , catMaybes
  , appendFoldable
  , mapWithIndex
  , sort
  , sortBy
  , module Exports
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.List ((:))
import Data.List as L
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.NonEmpty ((:|))
import Data.NonEmpty as NE
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Partial.Unsafe (unsafeCrashWith)

import Data.Foldable (foldl, foldr, foldMap, fold, intercalate, elem, notElem, find, findMap, any, all) as Exports
import Data.Traversable (scanl, scanr) as Exports

-- | Internal function: any structure-preserving operation on a list also
-- | applies to a NEL, this function is a helper for defining those cases.
wrappedOperation
  :: forall b a
   . String
  -> (L.List a -> L.List b)
  -> NonEmptyList a
  -> NonEmptyList b
wrappedOperation name f (NonEmptyList (x :| xs)) =
  case f (x : xs) of
    x' : xs' -> NonEmptyList (x' :| xs')
    L.Nil -> unsafeCrashWith ("Impossible: empty list in NonEmptyList." <> name)

toUnfoldable :: forall f. Unfoldable f => NonEmptyList ~> f
toUnfoldable =
  unfoldr (\xs -> (\rec -> Tuple rec.head rec.tail) <$> L.uncons xs) <<< toList

fromFoldable :: forall f a. Foldable f => f a -> Maybe (NonEmptyList a)
fromFoldable = fromList <<< L.fromFoldable

fromList :: forall a. L.List a -> Maybe (NonEmptyList a)
fromList L.Nil = Nothing
fromList (x : xs) = Just (NonEmptyList (x :| xs))

toList :: NonEmptyList ~> L.List
toList (NonEmptyList (x :| xs)) = x : xs

singleton :: forall a. a -> NonEmptyList a
singleton = NonEmptyList <<< NE.singleton

cons :: forall a. a -> NonEmptyList a -> NonEmptyList a
cons y (NonEmptyList (x :| xs)) = NonEmptyList (y :| x : xs)

snoc :: forall a. NonEmptyList a -> a -> NonEmptyList a
snoc (NonEmptyList (x :| xs)) y = NonEmptyList (x :| L.snoc xs y)

head :: forall a. NonEmptyList a -> a
head (NonEmptyList (x :| _)) = x

last :: forall a. NonEmptyList a -> a
last (NonEmptyList (x :| xs)) = fromMaybe x (L.last xs)

tail :: NonEmptyList ~> L.List
tail (NonEmptyList (_ :| xs)) = xs

init :: NonEmptyList ~> L.List
init (NonEmptyList (x :| xs)) = maybe L.Nil (x : _) (L.init xs)

uncons :: forall a. NonEmptyList a -> { head :: a, tail :: L.List a }
uncons (NonEmptyList (x :| xs)) = { head: x, tail: xs }

unsnoc :: forall a. NonEmptyList a -> { init :: L.List a, last :: a }
unsnoc (NonEmptyList (x :| xs)) = case L.unsnoc xs of
  Nothing -> { init: L.Nil, last: x }
  Just un -> { init: x : un.init, last: un.last }

length :: forall a. NonEmptyList a -> Int
length (NonEmptyList (x :| xs)) = 1 + L.length xs

reverse :: forall a. NonEmptyList a -> NonEmptyList a
reverse = wrappedOperation "reverse" L.reverse

filter :: forall a. (a -> Boolean) -> NonEmptyList a -> L.List a
filter f (NonEmptyList (x :| xs)) = L.filter f (x : xs)

filterM :: forall m a. Monad m => (a -> m Boolean) -> NonEmptyList a -> m (L.List a)
filterM f (NonEmptyList (x :| xs)) = L.filterM f (x : xs)

mapMaybe :: forall a b. (a -> Maybe b) -> NonEmptyList a -> L.List b
mapMaybe f (NonEmptyList (x :| xs)) = L.mapMaybe f (x : xs)

catMaybes :: forall a. NonEmptyList (Maybe a) -> L.List a
catMaybes (NonEmptyList (x :| xs)) = L.catMaybes (x : xs)

concatMap :: forall a b. (a -> NonEmptyList b) -> NonEmptyList a -> NonEmptyList b
concatMap = flip bind

appendFoldable :: forall t a. Foldable t => NonEmptyList a -> t a -> NonEmptyList a
appendFoldable (NonEmptyList (x :| xs)) ys =
  NonEmptyList (x :| (xs <> L.fromFoldable ys))

mapWithIndex :: forall a b. (Int -> a -> b) -> NonEmptyList a -> NonEmptyList b
mapWithIndex = wrappedOperation "mapWithIndex" <<< L.mapWithIndex

sort :: forall a. Ord a => NonEmptyList a -> NonEmptyList a
sort xs = sortBy compare xs

sortBy :: forall a. (a -> a -> Ordering) -> NonEmptyList a -> NonEmptyList a
sortBy = wrappedOperation "sortBy" <<< L.sortBy
