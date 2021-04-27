module Test.CommonDiffEmptiabilityRecord where

import Prelude

import Data.Foldable (class Foldable)
import Data.Function (on)
import Data.List as L
import Data.List.Lazy as LL
import Data.List.Lazy.NonEmpty as LNEL
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert)
import Test.Common (class Common, SkipBroken(..), assertSkipHelper, printTestType)

--type DeRec :: (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> Type
type DeRec c cInverse canEmpty nonEmpty cPattern =
  { makeCollection :: forall f a. Foldable f => f a -> c a

  , toCanEmpty :: forall a. c a -> canEmpty a
  , toNonEmpty :: forall a. c a -> nonEmpty a

  , catMaybes :: forall a. c (Maybe a) -> canEmpty a
  , drop :: forall a. Int -> c a -> canEmpty a
  , dropWhile :: forall a. (a -> Boolean) -> c a -> canEmpty a
  , filter :: forall a. (a -> Boolean) -> c a -> canEmpty a
  , filterM :: forall m a. Monad m => (a -> m Boolean) -> c a -> m (canEmpty a)
  , group :: forall a. Eq a => c a -> c (nonEmpty a)
  , groupAll :: forall a. Ord a => c a -> c (nonEmpty a)
  , groupBy :: forall a. (a -> a -> Boolean) -> c a -> c (nonEmpty a)
  , mapMaybe :: forall a b. (a -> Maybe b) -> c a -> canEmpty b
  , partition :: forall a. (a -> Boolean) -> c a -> { no :: canEmpty a, yes :: canEmpty a }
  , span :: forall a. (a -> Boolean) -> c a -> { init :: canEmpty a, rest :: canEmpty a }
  , take :: forall a. Int -> c a -> canEmpty a
  , takeEnd :: forall a. Int -> c a -> canEmpty a
  , takeWhile :: forall a. (a -> Boolean) -> c a -> canEmpty a

  , cons' :: forall a. a -> cInverse a -> c a
  , delete :: forall a. Eq a => a -> c a -> canEmpty a
  , deleteBy :: forall a. (a -> a -> Boolean) -> a -> c a -> canEmpty a
  , difference :: forall a. Eq a => c a -> c a -> canEmpty a
  , dropEnd :: forall a. Int -> c a -> canEmpty a
  -- There's a pending PR to update this signature
  -- groupAllBy :: forall a. (a -> a -> Ordering) -> c a -> c (nonEmpty a)
  , groupAllBy :: forall a. Ord a => (a -> a -> Boolean) -> c a -> c (nonEmpty a)
  , pattern :: forall a. c a -> cPattern a
  , slice :: Int -> Int -> c ~> canEmpty
  , snoc' :: forall a. cInverse a -> a -> c a
  , stripPrefix :: forall a. Eq a => cPattern a -> c a -> Maybe (canEmpty a)
}

testCommonDeRecBasic :: Effect Unit
testCommonDeRecBasic =
  testDeRec RunAll L.Nil L.Nil (NEL.singleton 1)
    { makeCollection = L.fromFoldable
    , toCanEmpty = identity
    , toNonEmpty = unsafePartial fromJust <<< NEL.fromList

    , catMaybes = L.catMaybes
    , drop = L.drop
    , dropWhile = L.dropWhile
    , filter = L.filter
    , filterM = L.filterM
    , group = L.group
    , groupAll = L.groupAll
    , groupBy = L.groupBy
    , mapMaybe = L.mapMaybe
    , partition = L.partition
    , span = L.span
    , take = L.take
    , takeEnd = L.takeEnd
    , takeWhile = L.takeWhile

    , cons' = L.cons'
    , delete = L.delete
    , deleteBy = L.deleteBy
    , difference = L.difference
    , dropEnd = L.dropEnd
    , groupAllBy = L.groupAllBy
    , pattern = L.Pattern
    , slice = L.slice
    , snoc' = L.snoc'
    , stripPrefix = L.stripPrefix
    }

testDeRec :: forall a c cInverse canEmpty nonEmpty cPattern.
  -- Common c =>
  -- CommonDiffEmptiability c cInverse canEmpty nonEmpty cPattern =>
  Eq (c (nonEmpty a)) =>
  Eq (canEmpty a) =>
  SkipBroken -> c a -> canEmpty a -> nonEmpty a ->
  DeRec c cInverse canEmpty nonEmpty cPattern ->
  Effect Unit
testDeRec skip _ nil _
  { makeCollection
  , toCanEmpty
  , toNonEmpty

  , catMaybes
  , drop
  , dropWhile
  , filter
  , filterM
  , group
  , groupAll
  , groupBy
  , mapMaybe
  , partition
  , span
  , take
  , takeEnd
  , takeWhile

  , cons'
  , delete
  , deleteBy
  , difference
  , dropEnd
  , groupAllBy
  , pattern
  , slice
  , snoc'
  , stripPrefix
  } = do
  let
    l :: forall f a. Foldable f => f a -> c a
    l = makeCollection

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