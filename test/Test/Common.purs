module Test.Common where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Extend (class Extend, (<<=))
import Control.Lazy (class Lazy)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array as Array
import Data.Eq (class Eq1, eq1)
import Data.Foldable (class Foldable, foldMap, foldl, sum)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.Function (on)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Int (odd)
import Data.List as L
import Data.List.Lazy as LL
import Data.List.Lazy.NonEmpty as LNEL
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid.Additive (Additive(..))
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)
import Data.Unfoldable1 (class Unfoldable1, unfoldr1)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert, assertEqual, assertEqual')

{-
This is temporarily being used during development.
It allows testing while still patching the API.
This is passed as an additional argument to testCommon
to indicate which collection type is being tested, and
lets us skip gaps that are currently implemented by `unsafeCrashWith`:

Once fully supported by all collections, can replace with original assert.
-}
data SkipBroken
  = SkipBrokenStrictCanEmpty
  | SkipBrokenStrictNonEmpty
  | SkipBrokenLazyCanEmpty
  | SkipBrokenLazyNonEmpty
  | RunAll

derive instance eqSkipBroken :: Eq SkipBroken

assertSkipHelper :: SkipBroken -> Array SkipBroken -> (_ -> Boolean) -> Effect Unit
assertSkipHelper skip arr f =
  case Array.elem skip arr of
    true -> log "...skipped"
    false -> assert $ f unit

printCollectionType :: String -> Effect Unit
printCollectionType str = do
  log "--------------------------------"
  log str
  log "--------------------------------"

printTestType :: String -> Effect Unit
printTestType str = do
  log $ "---- " <> str <> " Tests ----"

class (
  Alt c
  , Applicative c
  , Apply c
  , Bind c
  , Eq (c Int)
  , Eq1 c
  , Extend c
  , Foldable c
  , FoldableWithIndex Int c
  , Functor c
  , FunctorWithIndex Int c
  , Monad c
  , Ord (c Int)
  , Ord1 c
  , Semigroup (c Int)
  , Show (c Int)
  , Traversable c
  , TraversableWithIndex Int c
  , Unfoldable1 c
) <= Common c where
  makeCollection :: forall f a. Foldable f => f a -> c a

  concat :: forall a. c (c a) -> c a
  concatMap :: forall a. forall b. (a -> c b) -> c a -> c b
  cons :: forall a. a -> c a -> c a
  elemIndex :: forall a. Eq a => a -> c a -> Maybe Int
  elemLastIndex :: forall a. Eq a => a -> c a -> Maybe Int
  findIndex :: forall a. (a -> Boolean) -> c a -> Maybe Int
  findLastIndex :: forall a. (a -> Boolean) -> c a -> Maybe Int
  foldM :: forall m a b. Monad m => (b -> a -> m b) -> b -> c a -> m b
  index :: forall a. c a -> Int -> Maybe a
  intersect :: forall a. Eq a => c a -> c a -> c a
  intersectBy :: forall a. (a -> a -> Boolean) -> c a -> c a -> c a
  length :: forall a. c a -> Int
  nubEq :: forall a. Eq a => c a -> c a
  nubByEq :: forall a. (a -> a -> Boolean) -> c a -> c a
  range :: Int -> Int -> c Int
  reverse :: c ~> c
  singleton :: forall a. a -> c a
  snoc :: forall a. c a -> a -> c a
  toUnfoldable :: forall f a. Unfoldable f => c a -> f a
  union :: forall a. Eq a => c a -> c a -> c a
  unionBy :: forall a. (a -> a -> Boolean) -> c a -> c a -> c a
  unzip :: forall a b. c (Tuple a b) -> Tuple (c a) (c b)
  zip :: forall a b. c a -> c b -> c (Tuple a b)
  zipWith :: forall a b d. (a -> b -> d) -> c a -> c b -> c d
  zipWithA :: forall a b d m. Applicative m => (a -> b -> m d) -> c a -> c b -> m (c d)

  appendFoldable :: forall t a. Foldable t => c a -> t a -> c a
  insert :: forall a. Ord a => a -> c a -> c a
  insertBy :: forall a. (a -> a -> Ordering) -> a -> c a -> c a
  nub :: forall a. Ord a => c a -> c a
  nubBy :: forall a. (a -> a -> Ordering) -> c a -> c a
  some :: forall f a. Alternative f => Lazy (f (c a)) => f a -> f (c a)
  someRec :: forall f a. MonadRec f => Alternative f => f a -> f (c a)
  sort :: forall a. Ord a => c a -> c a
  sortBy :: forall a. (a -> a -> Ordering) -> c a -> c a
  transpose :: forall a. c (c a) -> c (c a)



-- Don't know how to define this in Test.Data.List
-- Wrapping is tricky.
instance commonList :: Common L.List where
  makeCollection = L.fromFoldable

  concat = L.concat
  concatMap = L.concatMap
  cons = L.Cons -- Should basic list have a cons function wrapping the Cons constructor?
  elemIndex = L.elemIndex
  elemLastIndex = L.elemLastIndex
  findIndex = L.findIndex
  findLastIndex = L.findLastIndex
  foldM = L.foldM
  index = L.index
  intersect = L.intersect
  intersectBy = L.intersectBy
  length = L.length
  nubEq = L.nubEq
  nubByEq = L.nubByEq
  range = L.range
  reverse = L.reverse
  singleton = L.singleton
  snoc = L.snoc
  toUnfoldable = L.toUnfoldable
  union = L.union
  unionBy = L.unionBy
  unzip = L.unzip
  zip = L.zip
  zipWith = L.zipWith
  zipWithA = L.zipWithA

  appendFoldable = L.appendFoldable
  insert = L.insert
  insertBy = L.insertBy
  nub = L.nub
  nubBy = L.nubBy
  some = L.some
  someRec = L.someRec
  sort = L.sort
  sortBy = L.sortBy
  transpose = L.transpose

instance commonNonEmptyList :: Common NEL.NonEmptyList where
  makeCollection = unsafePartial fromJust <<< NEL.fromFoldable

  concat = NEL.concat
  concatMap = NEL.concatMap
  cons = NEL.cons
  elemIndex = NEL.elemIndex
  elemLastIndex = NEL.elemLastIndex
  findIndex = NEL.findIndex
  findLastIndex = NEL.findLastIndex
  foldM = NEL.foldM
  index = NEL.index
  intersect = NEL.intersect
  intersectBy = NEL.intersectBy
  length = NEL.length
  nubEq = NEL.nubEq
  nubByEq = NEL.nubByEq
  range = NEL.range
  reverse = NEL.reverse
  singleton = NEL.singleton
  snoc = NEL.snoc
  toUnfoldable = NEL.toUnfoldable
  union = NEL.union
  unionBy = NEL.unionBy
  unzip = NEL.unzip
  zip = NEL.zip
  zipWith = NEL.zipWith
  zipWithA = NEL.zipWithA

  appendFoldable = NEL.appendFoldable
  insert = NEL.insert
  insertBy = NEL.insertBy
  nub = NEL.nub
  nubBy = NEL.nubBy
  some = NEL.some
  someRec = NEL.someRec
  sort = NEL.sort
  sortBy = NEL.sortBy
  transpose = NEL.transpose

instance commonLazyList :: Common LL.List where
  makeCollection = LL.fromFoldable

  concat = LL.concat
  concatMap = LL.concatMap
  cons = LL.cons
  elemIndex = LL.elemIndex
  elemLastIndex = LL.elemLastIndex
  findIndex = LL.findIndex
  findLastIndex = LL.findLastIndex
  foldM = LL.foldM
  index = LL.index
  intersect = LL.intersect
  intersectBy = LL.intersectBy
  length = LL.length
  nubEq = LL.nubEq
  nubByEq = LL.nubByEq
  range = LL.range
  reverse = LL.reverse
  singleton = LL.singleton
  snoc = LL.snoc
  toUnfoldable = LL.toUnfoldable
  union = LL.union
  unionBy = LL.unionBy
  unzip = LL.unzip
  zip = LL.zip
  zipWith = LL.zipWith
  zipWithA = LL.zipWithA

  appendFoldable = LL.appendFoldable
  insert = LL.insert
  insertBy = LL.insertBy
  nub = LL.nub
  nubBy = LL.nubBy
  some = LL.some
  someRec = LL.someRec
  sort = LL.sort
  sortBy = LL.sortBy
  transpose = LL.transpose

instance commonLazyNonEmptyList :: Common LNEL.NonEmptyList where
  makeCollection = unsafePartial fromJust <<< LNEL.fromFoldable

  concat = LNEL.concat
  concatMap = LNEL.concatMap
  cons = LNEL.cons
  elemIndex = LNEL.elemIndex
  elemLastIndex = LNEL.elemLastIndex
  findIndex = LNEL.findIndex
  findLastIndex = LNEL.findLastIndex
  foldM = LNEL.foldM
  index = LNEL.index
  intersect = LNEL.intersect
  intersectBy = LNEL.intersectBy
  length = LNEL.length
  nubEq = LNEL.nubEq
  nubByEq = LNEL.nubByEq
  range = LNEL.range
  reverse = LNEL.reverse
  singleton = LNEL.singleton
  snoc = LNEL.snoc
  toUnfoldable = LNEL.toUnfoldable
  union = LNEL.union
  unionBy = LNEL.unionBy
  unzip = LNEL.unzip
  zip = LNEL.zip
  zipWith = LNEL.zipWith
  zipWithA = LNEL.zipWithA

  appendFoldable = LNEL.appendFoldable
  insert = LNEL.insert
  insertBy = LNEL.insertBy
  nub = LNEL.nub
  nubBy = LNEL.nubBy
  some = LNEL.some
  someRec = LNEL.someRec
  sort = LNEL.sort
  sortBy = LNEL.sortBy
  transpose = LNEL.transpose

testCommon :: forall c.
  Common c =>
  Eq (c String) =>
  Eq (c (Tuple Int String)) =>
  Eq (c (c String)) =>
  Eq (c (Array Int)) =>
  Show (c String) =>
  Show (c (Tuple Int String)) =>
  Show (c (c String)) =>
  Show (c (Array Int)) =>
  c Int -> Effect Unit
-- Would likely be better to pass a proxy type
testCommon _ = do
  let
    l :: forall f a. Foldable f => f a -> c a
    l = makeCollection

    rg :: Int -> Int -> c Int
    rg = range

    bigCollection :: c _
    bigCollection = range 1 100000

  printTestType "Common"

  -- Duplicating this test out of alphabetical order, since many other tests rely on it.
  log "range should create an inclusive collection of integers for the specified start and end"
  assertEqual { actual: range 3 3, expected: l [3] }
  assertEqual { actual: range 0 5, expected: l [0, 1, 2, 3, 4, 5] }
  assertEqual { actual: range 2 (-3), expected: l [2, 1, 0, -1, -2, -3] }

  -- ======= Typeclass tests ========

  -- Alt
  --   alt :: forall a. f a -> f a -> f a
  -- Todo - Don't know in what situations this is different than append
  log "Alt's alt (<|>) should append collections"
  assertEqual { actual: l [1,2] <|> l [3,4], expected: l [1,2,3,4] }

  -- Applicative
  --   pure :: forall a. a -> f a
  log "Applicative's pure should construct a collection with a single value"
  assertEqual { actual: pure 5, expected: l [5] }

  -- Apply
  --   apply :: forall a b. f (a -> b) -> f a -> f b
  log "Apply's apply (<*>) should have cartesian product behavior for non-zippy collections"
  log "... skipped"
  -- Todo - make these consistent and also double-check for arrays
  -- can-empty behavior
  -- assertEqual { actual: makeCollection [mul 10, mul 100] <*> l [1, 2, 3], expected: l [10, 20, 30, 100, 200, 300] }
  -- NonEmpty behavior
  -- assertEqual { actual: makeCollection [mul 10, mul 100] <*> l [1, 2, 3], expected: l [10, 100, 20, 200, 30, 300] }

  -- Bind c
  --   bind :: forall a b. m a -> (a -> m b) -> m b
  log "Bind's bind (>>=) should append the results of a collection-generating function\
  \applied to each element in the collection"
  assertEqual { actual: l [1,2,3] >>= \x -> l [x,10+x], expected: l [1,11,2,12,3,13] }

  -- Eq
  --   eq :: a -> a -> Boolean
  log "Eq's eq (==) should correctly test collections for equality"
  assertEqual' "Equality failed" { actual: l [1,2] == l [1,2], expected: true }
  assertEqual' "Inequality failed" { actual: l [1,2] == l [2,2], expected: false }

  -- Eq1
  --   eq1 :: forall a. Eq a => f a -> f a -> Boolean
  log "Eq1's eq1 should correctly test collections for equality"
  assertEqual' "Equality failed" { actual: l [1,2] `eq1` l [1,2], expected: true }
  assertEqual' "Inequality failed" { actual: l [1,2] `eq1` l [2,2], expected: false }

  -- Extend
  --   extend :: forall b a. (w a -> b) -> w a -> w b
  log "Extend's extend (<<=) should create a collection containing the results\
  \of a function that is applied to increasingly smaller chunks of an input\
  \collection. Each iteration drops an element from the front of the input collection."
  assertEqual { actual: sum <<= l [1,2,3,4], expected: l [10,9,7,4] }

  -- Foldable
  --   foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  --   foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  --   foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
  -- These are just the pre-existing tests. They could be more comprehensive.

  log "foldl should be stack-safe"
  void $ pure $ foldl (+) 0 bigCollection

  log "foldMap should be stack-safe"
  void $ pure $ foldMap Additive bigCollection

  log "foldMap should be left-to-right"
  assertEqual { actual: foldMap show $ rg 1 5, expected: "12345" }

  -- FoldableWithIndex
  --   foldrWithIndex :: forall a b. (i -> a -> b -> b) -> b -> f a -> b
  --   foldlWithIndex :: forall a b. (i -> b -> a -> b) -> b -> f a -> b
  --   foldMapWithIndex :: forall a m. Monoid m => (i -> a -> m) -> f a -> m
  -- Todo - Existing tests, opportunities for improvement

  log "foldlWithIndex should be correct"
  assertEqual { actual: foldlWithIndex (\i b _ -> i + b) 0 $ rg 0 10000, expected: 50005000 }

  log "foldlWithIndex should be stack-safe"
  void $ pure $ foldlWithIndex (\i b _ -> i + b) 0 bigCollection

  log "foldrWithIndex should be correct"
  assertEqual { actual: foldrWithIndex (\i _ b -> i + b) 0 $ rg 0 10000, expected: 50005000 }

  log "foldrWithIndex should be stack-safe"
  void $ pure $ foldrWithIndex (\i _ b -> i + b) 0 bigCollection

  log "foldMapWithIndex should be stack-safe"
  void $ pure $ foldMapWithIndex (\i _ -> Additive i) bigCollection

  log "foldMapWithIndex should be left-to-right"
  assertEqual { actual: foldMapWithIndex (\i _ -> show i) (l [0, 0, 0]), expected: "012" }

  -- Functor
  --   map :: forall a b. (a -> b) -> f a -> f b

  log "map should maintain order"
  assertEqual { actual: rg 1 5, expected: map identity $ rg 1 5 }

  log "map should be stack-safe"
  void $ pure $ map identity bigCollection
  -- Todo - The below test also performs the same stack-safety check

  log "map should be correct"
  assertEqual { actual: rg 1 100000, expected: map (_ + 1) $ rg 0 99999 }


  -- FunctorWithIndex
  --   mapWithIndex :: forall a b. (i -> a -> b) -> f a -> f b
  -- Todo - improve pre-existing

  log "mapWithIndex should take a collection of values and apply a function which also takes the index into account"
  assertEqual { actual: mapWithIndex add $ l [0, 1, 2, 3], expected: l [0, 2, 4, 6] }

  -- Monad
  --   Indicates Applicative and Bind, which are already tested

  -- Ord
  --   compare :: a -> a -> Ordering
  -- Todo - add tests

  -- Ord1
  --   compare1 :: forall a. Ord a => f a -> f a -> Ordering
  -- Todo - add tests

  -- Semigroup
  --   append :: a -> a -> a

  log "append should concatenate two collections"
  assertEqual { actual: l [1, 2] <> l [3, 4], expected: l [1, 2, 3, 4] }

  log "append should be stack-safe"
  void $ pure $ bigCollection <> bigCollection

  -- Show
  --   show :: a -> String
  -- This is not testable in a generic way

  -- Traversable
  --   traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
  --   sequence :: forall a m. Applicative m => t (m a) -> m (t a)
  -- Todo - improve pre-existing tests
  -- Todo - add sequence test

  log "traverse should be stack-safe"
  assertEqual { actual: traverse Just bigCollection, expected: Just bigCollection }

  -- TraversableWithIndex
  --   traverseWithIndex :: forall a b m. Applicative m => (i -> a -> m b) -> t a -> m (t b)

  log "traverseWithIndex should be stack-safe"
  assertEqual { actual: traverseWithIndex (const Just) bigCollection, expected: Just bigCollection }

  log "traverseWithIndex should be correct"
  assertEqual { actual: traverseWithIndex (\i a -> Just $ i + a) (l [2, 2, 2]), expected: Just $ l [2, 3, 4] }

  -- Unfoldable1
  --   unfoldr1 :: forall a b. (b -> Tuple a (Maybe b)) -> b -> t a

  let
    step1 :: Int -> Tuple Int (Maybe Int)
    step1 n = Tuple n $ if n >= 5 then Nothing else Just $ n + 1

  log "unfoldr1 should maintain order"
  assertEqual { actual: rg 1 5, expected: unfoldr1 step1 1 }

  -- ===========   Functions   ===========

  -- Todo - split
  -- log "catMaybe should take a collection of Maybe values and throw out Nothings"
  -- assertEqual { actual: catMaybes (l [Nothing, Just 2, Nothing, Just 4]), expected: l [2, 4] }

  log "concat should join a collection of collections"
  assertEqual { actual: concat $ l [l [1, 2], l [3, 4]], expected: l [1, 2, 3, 4] }

  let
    doubleAndOrig :: Int -> c Int
    doubleAndOrig x = cons (x * 2) $ singleton x

  log "concatMap should be equivalent to (concat <<< map)"
  assertEqual { actual: concatMap doubleAndOrig $ l [1, 2, 3], expected: concat $ map doubleAndOrig $ l [1, 2, 3] }

  log "cons should add an element to the front of the collection"
  assertEqual { actual: cons 1 $ l [2, 3], expected: l [1,2,3] }

  log "elemIndex should return the index of an item that a predicate returns true for in a collection"
  assertEqual { actual: elemIndex 1 $ l [1, 2, 1], expected: Just 0 }
  assertEqual { actual: elemIndex 4 $ l [1, 2, 1], expected: Nothing }

  log "elemLastIndex should return the last index of an item in a collection"
  assertEqual { actual: elemLastIndex 1 $ l [1, 2, 1], expected: Just 2 }
  assertEqual { actual: elemLastIndex 4 $ l [1, 2, 1], expected: Nothing }

  -- Todo split
  -- log "filter should remove items that don't match a predicate"
  -- assertEqual { actual: filter odd $ range 0 10, expected: l [1, 3, 5, 7, 9] }

  --log "filterM should remove items that don't match a predicate while using a monadic behaviour"
  --assertEqual { actual: filterM (Just <<< odd) $ range 0 10, expected: Just $ l [1, 3, 5, 7, 9] }
  --assertEqual { actual: filterM (const Nothing) $ rg 0 10, expected: Nothing }

  log "findIndex should return the index of an item that a predicate returns true for in a collection"
  assertEqual { actual: findIndex (_ /= 1) $ l [1, 2, 1], expected: Just 1 }
  assertEqual { actual: findIndex (_ == 3) $ l [1, 2, 1], expected: Nothing }

  log "findLastIndex should return the last index of an item in a collection"
  assertEqual { actual: findLastIndex (_ /= 1) $ l [2, 1, 2], expected: Just 2 }
  assertEqual { actual: findLastIndex (_ == 3) $ l [2, 1, 2], expected: Nothing }

  log "foldM should perform a fold using a monadic step function"
  assertEqual { actual: foldM (\x y -> Just $ x + y) 0 $ rg 1 10, expected: Just 55 }
  assertEqual { actual: foldM (\_ _ -> Nothing) 0 $ rg 1 10, expected: Nothing }

  log "index (!!) should return Just x when the index is within the bounds of the collection"
  assertEqual { actual: l [1, 2, 3] `index` 0, expected: Just 1 }
  assertEqual { actual: l [1, 2, 3] `index` 1, expected: Just 2 }
  assertEqual { actual: l [1, 2, 3] `index` 2, expected: Just 3 }

  log "index (!!) should return Nothing when the index is outside of the bounds of the collection"
  assertEqual { actual: l [1, 2, 3] `index` 6, expected: Nothing }
  assertEqual { actual: l [1, 2, 3] `index` (-1), expected: Nothing }

  -- todo split
  -- log "insertAt should add an item at the specified index"
  -- assertEqual { actual: insertAt 0 1 $ l [2, 3], expected: Just $ l [1, 2, 3] }
  -- assertEqual { actual: insertAt 1 1 $ l [2, 3], expected: Just $ l [2, 1, 3] }
  -- assertEqual { actual: insertAt 2 1 $ l [2, 3], expected: Just $ l [2, 3, 1] }

  -- log "insertAt should return Nothing if the index is out of range"
  -- assertEqual { actual: insertAt 7 8 $ l [1,2,3], expected: Nothing }

  log "intersect should return the intersection of two collections"
  assertEqual { actual: intersect (l [1, 2, 3, 4, 3, 2, 1]) $ l [1, 1, 2, 3], expected: l [1, 2, 3, 3, 2, 1] }

  log "intersectBy should return the intersection of two collections using the specified equivalence relation"
  assertEqual { actual: intersectBy (\x y -> x * 2 == y) (l [1, 2, 3]) $ l [2, 6], expected: l [1, 3] }

  log "length should return the number of items in a collection"
  assertEqual { actual: length $ l [1], expected: 1 }
  assertEqual { actual: length $ l [1, 2, 3, 4, 5], expected: 5 }

  log "length should be stack-safe"
  void $ pure $ length bigCollection

  -- todo split
  -- log "modifyAt should update an item at the specified index"
  -- assertEqual { actual: modifyAt 0 (_ + 1) $ l [1, 2, 3], expected: Just $ l [2, 2, 3] }
  -- assertEqual { actual: modifyAt 1 (_ + 1) $ l [1, 2, 3], expected: Just $ l [1, 3, 3] }

  -- log "modifyAt should return Nothing if the index is out of range"
  -- assertEqual { actual: modifyAt 7 (_ + 1) $ l [1,2,3], expected: Nothing }

  log "nubEq should remove duplicate elements from the collection, keeping the first occurence"
  assertEqual { actual: nubEq $ l [1, 2, 2, 3, 4, 1], expected: l [1, 2, 3, 4] }

  log "nubByEq should remove duplicate items from the collection using a supplied predicate"
  let mod3eq = eq `on` \n -> mod n 3
  assertEqual { actual: nubByEq mod3eq $ l [1, 3, 4, 5, 6], expected: l [1, 3, 5] }

  log "range should create an inclusive collection of integers for the specified start and end"
  assertEqual { actual: range 3 3, expected: l [3] }
  assertEqual { actual: range 0 5, expected: l [0, 1, 2, 3, 4, 5] }
  assertEqual { actual: range 2 (-3), expected: l [2, 1, 0, -1, -2, -3] }

  log "reverse should reverse the order of items in a collection"
  assertEqual { actual: reverse $ l [1, 2, 3], expected: l [3, 2, 1] }

  log "singleton should construct a collection with a single value"
  assertEqual { actual: singleton 5, expected: l [5] }

  log "snoc should add an item to the end of a collection"
  assertEqual { actual: l [1, 2, 3] `snoc` 4, expected: l [1, 2, 3, 4] }

  -- Todo toUnfoldable

  log "union should produce the union of two collections"
  assertEqual { actual: union (l [1, 2, 3]) $ l [2, 3, 4], expected: l [1, 2, 3, 4] }
  assertEqual { actual: union (l [1, 1, 2, 3]) $ l [2, 3, 4], expected: l [1, 1, 2, 3, 4] }

  log "unionBy should produce the union of two collections using the specified equality relation"
  assertEqual { actual: unionBy (\_ y -> y < 5) (l [1, 2, 3]) $ l [2, 3, 4, 5, 6], expected: l [1, 2, 3, 5, 6] }

  log "unzip should deconstruct a collection of tuples into a tuple of collections"
  assertEqual { actual: unzip $ l [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"], expected: Tuple (l [1, 2, 3]) $ l ["a", "b", "c"] }

  log "zip should use the specified function to zip two collections together"
  assertEqual { actual: zip (l [1, 2, 3]) $ l ["a", "b", "c"], expected: l [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"] }

  log "zipWith should use the specified function to zip two collections together"
  assertEqual { actual: zipWith (\x y -> l [show x, y]) (l [1, 2, 3]) $ l ["a", "b", "c"], expected: l [l ["1", "a"], l ["2", "b"], l ["3", "c"]] }

  log "zipWithA should use the specified function to zip two collections together"
  assertEqual { actual: zipWithA (\x y -> Just $ Tuple x y) (l [1, 2, 3]) $ l ["a", "b", "c"], expected: Just $ l [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"] }

  {-
  New stuff
  Todo:
  -- convert to assertEqual
  -- sort into above
  -}

  -- appendFoldable :: forall t a. Foldable t => c a -> t a -> c a
  -- todo

  {-
  -- Todo - clean these up

  log "insert should add an item at the appropriate place in a sorted list"
  assert $ insert 2 (l [1, 1, 3]) == l [1, 1, 2, 3]
  assert $ insert 4 (l [1, 2, 3]) == l [1, 2, 3, 4]
  assert $ insert 0 (l [1, 2, 3]) == l [0, 1, 2, 3]

  log "insertBy should add an item at the appropriate place in a sorted list using the specified comparison"
  assert $ insertBy (flip compare) 4 (l [1, 2, 3]) == l [4, 1, 2, 3]
  assert $ insertBy (flip compare) 0 (l [1, 2, 3]) == l [1, 2, 3, 0]

  -- nub :: forall a. Ord a => c a -> c a
  -- nubBy :: forall a. (a -> a -> Ordering) -> c a -> c a

  log "nub should remove duplicate elements from the list, keeping the first occurrence"
  assert $ nub (l [1, 2, 2, 3, 4, 1]) == l [1, 2, 3, 4]

  log "nubBy should remove duplicate items from the list using a supplied predicate"
  assert $ nubBy (compare `on` Array.length) (l [[1],[2],[3,4]]) == l [[1],[3,4]]
  -}


  {-
  -- replicate :: forall a. Int -> a -> c a
  log "unfoldable replicate should be stack-safe"
  void $ pure $ length $ replicate 100000 1

  log "replicate should produce an list containing an item a specified number of times"
  assert $ replicate 3 true == l [true, true, true]
  assert $ replicate 1 "foo" == l ["foo"]
  assert $ replicate 0 "foo" == l []
  assert $ replicate (-1) "foo" == l []

  log "replicateA should perform the monadic action the correct number of times"
  assert $ replicateA 3 (Just 1) == Just (l [1, 1, 1])
  assert $ replicateA 1 (Just 1) == Just (l [1])
  assert $ replicateA 0 (Just 1) == Just (l [])
  assert $ replicateA (-1) (Just 1) == Just (l [])
  -}




  -- replicateM :: forall m a. Monad m => Int -> m a -> m (c a)
  -- some :: forall f a. Alternative f => Lazy (f (c a)) => f a -> f (c a)
  -- someRec :: forall f a. MonadRec f => Alternative f => f a -> f (c a)
  -- sort :: forall a. Ord a => c a -> c a
  -- sortBy :: forall a. (a -> a -> Ordering) -> c a -> c a
  -- transpose :: forall a. c (c a) -> c (c a)

