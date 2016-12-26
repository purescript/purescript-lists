module Data.List.NonEmpty
  ( module Data.List.Types
  , toUnfoldable
  , fromFoldable
  , fromList
  , toList
  , singleton
  , head
  , tail
  , init
  , uncons
  , length
  , concatMap
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.List ((:))
import Data.List as L
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.NonEmpty ((:|))
import Data.NonEmpty as NE
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)

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

head :: forall a. NonEmptyList a -> a
head (NonEmptyList (x :| _)) = x

last :: forall a. NonEmptyList a -> a
last (NonEmptyList (x :| xs)) = fromMaybe x (L.last xs)

tail :: forall a. NonEmptyList a -> L.List a
tail (NonEmptyList (_ :| xs)) = xs

init :: forall a. NonEmptyList a -> L.List a
init (NonEmptyList (x :| xs)) = maybe L.Nil (x : _) (L.init xs)

uncons :: forall a. NonEmptyList a -> { head :: a, tail :: L.List a }
uncons (NonEmptyList (x :| xs)) = { head: x, tail: xs }

length :: forall a. NonEmptyList a -> Int
length (NonEmptyList (x :| xs)) = 1 + L.length xs

concatMap :: forall a b. (a -> NonEmptyList b) -> NonEmptyList a -> NonEmptyList b
concatMap = flip bind
