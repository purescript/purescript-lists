module Test.CommonDiffEmptiability where

import Prelude

import Data.Foldable (class Foldable)
import Data.Function (on)
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert)

import Test.Common (class Common, SkipBroken(..), assertSkipHelper, printTestType, makeContainer, range)

import Data.List as L
import Data.List.NonEmpty as NEL
import Data.List.Lazy as LL
import Data.List.Lazy.NonEmpty as LNEL

{-
This is for testing common functions that have slightly different
signatures depending on whether the container may be empty or not.
For example:
 CanEmpty (as `c`):
  drop :: forall a. Int -> c a -> c a
  fromFoldable :: forall f. Foldable f => f ~> c
  group :: forall a. Eq a => c a -> c (nonEmpty a)
  head :: forall a. c a -> Maybe a
 NonEmpty (as `c`):
  drop :: forall a. Int -> c a -> canEmpty a
  fromFoldable :: forall f a. Foldable f => f a -> Maybe (c a)
  group :: forall a. Eq a => c a -> c (c a)
  head :: forall a. c a -> a

These are consolidated by providing different type constructors to the typeclass instances.

This generally works, but cannot be done if `Maybe` is present in one of the versions.
So functions like `fromFoldable` and `head` must be tested elswhere with some duplication.
The original plan was to pass another function with the same kind signature as `Maybe`,
such as:
  type Id x = x
But creating an "identity" type alias doesn't work because:
  - First-class type families are required:
    - https://stackoverflow.com/questions/63865620/can-haskell-type-synonyms-be-used-as-type-constructors
  - Typeclasses only match on type constructors and not any arbritrary
  type-level function with the same kind signature.
    - https://old.reddit.com/r/haskell/comments/26dshj/why_doesnt_haskell_allow_type_aliases_in_the/
-}


class (
  Eq (c Int)
) <= CommonDiffEmptiability c canEmpty nonEmpty | c -> canEmpty nonEmpty where

  toCanEmpty :: forall a. c a -> canEmpty a
  toNonEmpty :: forall a. c a -> nonEmpty a

  catMaybes :: forall a. c (Maybe a) -> canEmpty a
  drop :: forall a. Int -> c a -> canEmpty a
  dropWhile :: forall a. (a -> Boolean) -> c a -> canEmpty a
  filter :: forall a. (a -> Boolean) -> c a -> canEmpty a
  filterM :: forall m a. Monad m => (a -> m Boolean) -> c a -> m (canEmpty a)
  group :: forall a. Eq a => c a -> c (nonEmpty a)
  groupAll :: forall a. Ord a => c a -> c (nonEmpty a)
  groupBy :: forall a. (a -> a -> Boolean) -> c a -> c (nonEmpty a)
  mapMaybe :: forall a b. (a -> Maybe b) -> c a -> canEmpty b
  partition :: forall a. (a -> Boolean) -> c a -> { no :: canEmpty a, yes :: canEmpty a }
  span :: forall a. (a -> Boolean) -> c a -> { init :: canEmpty a, rest :: canEmpty a }
  take :: forall a. Int -> c a -> canEmpty a
  takeEnd :: forall a. Int -> c a -> canEmpty a
  takeWhile :: forall a. (a -> Boolean) -> c a -> canEmpty a


instance commonDiffEmptiabilityCanEmptyList :: CommonDiffEmptiability L.List L.List NEL.NonEmptyList where

  toCanEmpty = identity
  toNonEmpty = unsafePartial fromJust <<< NEL.fromList

  catMaybes = L.catMaybes
  drop = L.drop
  dropWhile = L.dropWhile
  filter = L.filter
  filterM = L.filterM
  group = L.group
  groupAll = L.groupAll
  groupBy = L.groupBy
  mapMaybe = L.mapMaybe
  partition = L.partition
  span = L.span
  take = L.take
  takeEnd = L.takeEnd
  takeWhile = L.takeWhile

instance commonDiffEmptiabilityNonEmptyList :: CommonDiffEmptiability NEL.NonEmptyList L.List NEL.NonEmptyList where

  toCanEmpty = NEL.toList
  toNonEmpty = identity

  catMaybes = NEL.catMaybes
  drop = NEL.drop
  dropWhile = NEL.dropWhile
  filter = NEL.filter
  filterM = NEL.filterM
  group = NEL.group
  groupAll = NEL.groupAll
  groupBy = NEL.groupBy
  mapMaybe = NEL.mapMaybe
  partition = NEL.partition
  span = NEL.span
  take = NEL.take
  takeEnd = NEL.takeEnd
  takeWhile = NEL.takeWhile

instance commonDiffEmptiabilityCanEmptyLazyList :: CommonDiffEmptiability LL.List LL.List LNEL.NonEmptyList where

  toCanEmpty = identity
  toNonEmpty = unsafePartial fromJust <<< LNEL.fromList

  catMaybes = LL.catMaybes
  drop = LL.drop
  dropWhile = LL.dropWhile
  filter = LL.filter
  filterM = LL.filterM
  group = LL.group
  groupAll = LL.groupAll
  groupBy = LL.groupBy
  mapMaybe = LL.mapMaybe
  partition = LL.partition
  span = LL.span
  take = LL.take
  takeEnd = LL.takeEnd
  takeWhile = LL.takeWhile

instance commonDiffEmptiabilityLazyNonEmptyList :: CommonDiffEmptiability LNEL.NonEmptyList LL.List LNEL.NonEmptyList where

  toCanEmpty = LNEL.toList
  toNonEmpty = identity

  catMaybes = LNEL.catMaybes
  drop = LNEL.drop
  dropWhile = LNEL.dropWhile
  filter = LNEL.filter
  filterM = LNEL.filterM
  group = LNEL.group
  groupAll = LNEL.groupAll
  groupBy = LNEL.groupBy
  mapMaybe = LNEL.mapMaybe
  partition = LNEL.partition
  span = LNEL.span
  take = LNEL.take
  takeEnd = LNEL.takeEnd
  takeWhile = LNEL.takeWhile

testCommonDiffEmptiability :: forall c canEmpty nonEmpty.
  Common c =>
  CommonDiffEmptiability c canEmpty nonEmpty =>
  Eq (c (nonEmpty Int)) =>
  Eq (canEmpty Int) =>
  SkipBroken -> c Int -> canEmpty Int -> nonEmpty Int -> Effect Unit
testCommonDiffEmptiability skip _ nil _ = do
  let
    l :: forall f a. Foldable f => f a -> c a
    l = makeContainer

    cel :: forall f a. Foldable f => f a -> canEmpty a
    cel = toCanEmpty <<< l

    nel :: forall f a. Foldable f => f a -> nonEmpty a
    nel = toNonEmpty <<< l

    assertSkip :: Array SkipBroken -> (_ -> Boolean) -> Effect Unit
    assertSkip = assertSkipHelper skip

  printTestType "Common (where signatures differ based on emptiability)"

  --catMaybes :: forall a. c (Maybe a) -> c a
  -- todo

  log "drop should remove the specified number of items from the front of an list"
  assert $ (drop 1 (l [1, 2, 3])) == cel [2, 3]
  assert $ (drop (-1) (l [1, 2, 3])) == cel [1, 2, 3]

  log "dropWhile should remove all values that match a predicate from the front of an list"
  assert $ (dropWhile (_ /= 1) (l [1, 2, 3])) == cel [1, 2, 3]
  assert $ (dropWhile (_ /= 2) (l [1, 2, 3])) == cel [2, 3]
  --assert $ (dropWhile (_ /= 1) nil) == nil

  --filter :: forall a. (a -> Boolean) -> c a -> c a
  -- todo

  --filterM :: forall m a. Monad m => (a -> m Boolean) -> c a -> m (c a)
  -- todo

  log "group should group consecutive equal elements into lists"
  assert $ group (l [1, 2, 2, 3, 3, 3, 1]) == l [nel [1], nel [2, 2], nel [3, 3, 3], nel [1]]

  log "groupAll should group equal elements into lists"
  assertSkip [SkipBrokenLazyCanEmpty]
   \_ -> groupAll (l [1, 2, 2, 3, 3, 3, 1]) == l [nel [1, 1], nel [2, 2], nel [3, 3, 3]]
  --assert $ groupAll (l [1, 2, 2, 3, 3, 3, 1]) == l [nel [1, 1], nel [2, 2], nel [3, 3, 3]]

  log "groupBy should group consecutive equal elements into lists based on an equivalence relation"
  assert $ groupBy (eq `on` (_ `mod` 10)) (l [1, 2, 12, 3, 13, 23, 11]) == l [nel [1], nel [2, 12], nel [3, 13, 23], nel [11]]

  -- todo - wait for this to be reworked
  -- log "groupAllBy should group equal elements into lists based on an comparison function"
  --assert $ groupAllBy (compare `on` mod 10) (l [1, 2, 12, 3, 13, 23, 11]) == l [nel [1, 11], nel [2, 12], nel [3, 13, 23]]

  log "mapMaybe should transform every item in an list, throwing out Nothing values"
  assert $ mapMaybe (\x -> if x /= 0 then Just x else Nothing) (l [0, 1, 0, 0, 2, 3]) == cel [1, 2, 3]

  log "partition should separate a list into a tuple of lists that do and do not satisfy a predicate"
  let partitioned = partition (_ > 2) (l [1, 5, 3, 2, 4])
  assert $ partitioned.yes == cel [5, 3, 4]
  assert $ partitioned.no == cel [1, 2]

  log "span should split an list in two based on a predicate"
  let spanResult = span (_ < 4) (l [1, 2, 3, 4, 5, 6, 7])
  assert $ spanResult.init == cel [1, 2, 3]
  assert $ spanResult.rest == cel [4, 5, 6, 7]

  log "take should keep the specified number of items from the front of an list, discarding the rest"
  assert $ (take 1 (l [1, 2, 3])) == cel [1]
  assert $ (take 2 (l [1, 2, 3])) == cel [1, 2]
  --assert $ (take 1 nil) == nil
  assert $ (take 0 (l [1, 2])) == nil
  assert $ (take (-1) (l [1, 2])) == nil

  log "takeEnd should keep the specified number of items from the end of an list, discarding the rest"
  assertSkip [SkipBrokenLazyCanEmpty]
    \_ -> (takeEnd 1 (l [1, 2, 3])) == cel [3]
  assertSkip [SkipBrokenLazyCanEmpty]
    \_ -> (takeEnd 2 (l [1, 2, 3])) == cel [2, 3]
  assertSkip [SkipBrokenLazyCanEmpty]
    \_ -> (takeEnd 2 (l [1])) == cel [1]

  --assert $ (takeEnd 1 (l [1, 2, 3])) == cel [3]
  --assert $ (takeEnd 2 (l [1, 2, 3])) == cel [2, 3]
  ----assert $ (takeEnd 1 nil) == nil
  --assert $ (takeEnd 2 (l [1])) == cel [1]

  log "takeWhile should keep all values that match a predicate from the front of an list"
  assert $ (takeWhile (_ /= 2) (l [1, 2, 3])) == cel [1]
  assert $ (takeWhile (_ /= 3) (l [1, 2, 3])) == cel [1, 2]
  --assert $ (takeWhile (_ /= 1) nil) == nil
