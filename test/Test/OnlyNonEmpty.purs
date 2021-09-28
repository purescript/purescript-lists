module Test.OnlyNonEmpty where

import Prelude

import Control.Comonad (class Comonad)
import Data.Foldable (class Foldable, foldMap, foldl)
import Data.List as L
import Data.List.Lazy as LL
import Data.List.Lazy.NonEmpty as LNEL
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.Semigroup.Foldable (class Foldable1)
import Data.Semigroup.Traversable (class Traversable1)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert, assertEqual)
import Test.Common (class Common, SkipBroken(..), assertSkipHelper, printTestType, makeCollection)

class (
  Comonad c
  --, Foldable1 c -- missing from LazyNonEmptyList
  --, Traversable1 c -- missing from LazyNonEmptyList
) <= OnlyNonEmpty c canEmpty | c -> canEmpty, canEmpty -> c where

  makeCanEmptyCollection :: forall f a. Foldable f => f a -> canEmpty a

  -- These are the same function names as the CanEmpty versions,
  -- but the signatures are different and can't be merged in the
  -- CommonDiffEmptiability tests. This is due to a mismatch in the
  -- presence of `Maybe`s.

  fromFoldable :: forall f a. Foldable f => f a -> Maybe (c a)
  head :: forall a. c a -> a
  init :: forall a. c a -> canEmpty a
  last :: forall a. c a -> a
  tail :: forall a. c a -> canEmpty a
  uncons :: forall a. c a -> { head :: a, tail :: canEmpty a }

  -- These are only available for NonEmpty collections

  fromList :: forall a. canEmpty a -> Maybe (c a)
  toList :: c ~> canEmpty

instance onlyNonEmptyList :: OnlyNonEmpty NEL.NonEmptyList L.List where

  makeCanEmptyCollection = L.fromFoldable

  fromFoldable = NEL.fromFoldable
  head = NEL.head
  init = NEL.init
  last = NEL.last
  tail = NEL.tail
  uncons = NEL.uncons

  fromList = NEL.fromList
  toList = NEL.toList

instance onlyNonEmptyLazyList :: OnlyNonEmpty LNEL.NonEmptyList LL.List where

  makeCanEmptyCollection = LL.fromFoldable

  fromFoldable = LNEL.fromFoldable
  head = LNEL.head
  init = LNEL.init
  last = LNEL.last
  tail = LNEL.tail
  uncons = LNEL.uncons

  fromList = LNEL.fromList
  toList = LNEL.toList

testOnlyNonEmpty :: forall c canEmpty.
  Common c =>
  OnlyNonEmpty c canEmpty =>
  Eq (c Int) =>
  Eq (canEmpty Int) =>
  Show (canEmpty Int) =>
  c Int -> canEmpty Int -> Effect Unit
testOnlyNonEmpty _ _ = do
  let
    l :: forall f a. Foldable f => f a -> c a
    l = makeCollection

    cel :: forall f a. Foldable f => f a -> canEmpty a
    cel = makeCanEmptyCollection

  printTestType "Only nonEmpty"

  -- ======= Typeclass tests ========

  -- Todo

  -- Comonad
  -- Foldable1
  -- Traversable1

  -- ======= Functions tests ========

  log "fromList should convert from a List to a NonEmptyList"
  assertEqual { actual: fromList $ cel [1, 2, 3], expected: Just $ l [1, 2, 3] }
  assertEqual { actual: fromList $ cel ([] :: _ Int), expected: Nothing }

  log "toList should convert from a NonEmptyList to a List"
  assertEqual { actual: toList $ l [1, 2, 3], expected: cel [1, 2, 3] }


  -- These are the remaining functions that can't be deduplicated due to use of Maybe

  log "head should return a the first value"
  assert $ head (l [1, 2]) == 1

  log "init should return a canEmpty collection of all but the last value"
  assert $ init (l [1, 2, 3]) == cel [1, 2]

  log "last should return the last value"
  assert $ last (l [1, 2]) == 2

  log "tail should return a canEmpty collection of all but the first value"
  assert $ tail (l [1, 2, 3]) == cel [2, 3]

  log "uncons should split a collection into a record containing the first and remaining values"
  assert $ uncons (l [1]) == {head: 1, tail: cel []}
  assert $ uncons (l [1, 2, 3]) == {head: 1, tail: cel [2, 3]}
