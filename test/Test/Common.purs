module Test.Common where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Extend (class Extend, (<<=))
import Control.Lazy (class Lazy)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array as Array
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMap, foldl, sum)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.Function (on)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Int (odd)
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid.Additive (Additive(..))
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, replicate, replicateA, unfoldr)
import Data.Unfoldable1 (class Unfoldable1, unfoldr1)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert)

import Data.List as L
import Data.List.NonEmpty as NEL
import Data.List.Lazy as LL
import Data.List.Lazy.NonEmpty as LNEL

{-
This is temporarily being used during development.
It allows testing while still patching the API.
This is passed as an additional argument to testCommon
to indicate which container type is being tested, and
lets us skip gaps that are currently implemented by `unsafeCrashWith`:

Once fully supported by all containers, can replace with original assert.
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

printContainerType :: String -> Effect Unit
printContainerType str = do
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
  --, Eq1 c -- missing from NonEmptyList, LazyNonEmptyList
  , Extend c
  , Foldable c
  , FoldableWithIndex Int c
  , Functor c
  , FunctorWithIndex Int c
  , Monad c
  , Ord (c Int)
  --, Ord1 c -- missing from NonEmptyList, LazyNonEmptyList
  , Semigroup (c Int)
  , Show (c Int)
  , Traversable c
  , TraversableWithIndex Int c
  , Unfoldable1 c
) <= Common c where
  makeContainer :: forall f a. Foldable f => f a -> c a

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
  -- This constructor is probably best to be set in diff empty
  -- pattern :: forall a. (c a) -> Pattern a
  replicate :: forall a. Int -> a -> c a
  replicateM :: forall m a. Monad m => Int -> m a -> m (c a)
  some :: forall f a. Alternative f => Lazy (f (c a)) => f a -> f (c a)
  someRec :: forall f a. MonadRec f => Alternative f => f a -> f (c a)
  sort :: forall a. Ord a => c a -> c a
  sortBy :: forall a. (a -> a -> Ordering) -> c a -> c a
  transpose :: forall a. c (c a) -> c (c a)




-- Don't know how to define this in Test.Data.List
-- Wrapping is tricky.
instance commonList :: Common L.List where
  makeContainer = L.fromFoldable

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
  -- pattern = L.Pattern
  replicate = L.replicate
  replicateM = L.replicateM
  some = L.some
  someRec = L.someRec
  sort = L.sort
  sortBy = L.sortBy
  transpose = L.transpose

instance commonNonEmptyList :: Common NEL.NonEmptyList where
  makeContainer = unsafePartial fromJust <<< NEL.fromFoldable

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
  --pattern = NEL.Pattern
  replicate = NEL.replicate
  replicateM = NEL.replicateM
  some = NEL.some
  someRec = NEL.someRec
  sort = NEL.sort
  sortBy = NEL.sortBy
  transpose = NEL.transpose

instance commonLazyList :: Common LL.List where
  makeContainer = LL.fromFoldable

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
  --pattern = LL.Pattern
  replicate = LL.replicate
  replicateM = LL.replicateM
  some = LL.some
  someRec = LL.someRec
  sort = LL.sort
  sortBy = LL.sortBy
  transpose = LL.transpose

instance commonLazyNonEmptyList :: Common LNEL.NonEmptyList where
  makeContainer = unsafePartial fromJust <<< LNEL.fromFoldable

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
  -- pattern = LNEL.Pattern
  replicate = LNEL.replicate
  replicateM = LNEL.replicateM
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
  c Int -> Effect Unit
-- Would likely be better to pass a proxy type
testCommon _ = do
  let
    l :: forall f a. Foldable f => f a -> c a
    l = makeContainer

    rg :: Int -> Int -> c Int
    rg = range

    k100 :: c _
    k100 = range 1 100000

  printTestType "Common"

  -- Duplicating this test out of alphabetical order, since many other tests rely on it.
  log "range should create an inclusive container of integers for the specified start and end"
  assert $ (range 3 3) == l [3]
  --assertSkip \_ -> (range 3 3) == l [3]
  assert $ (range 0 5) == l [0, 1, 2, 3, 4, 5]
  assert $ (range 2 (-3)) == l [2, 1, 0, -1, -2, -3]

  -- ======= Typeclass tests ========

  -- Alt
  --   alt :: forall a. f a -> f a -> f a
  -- Don't know in what situations this is different than append
  log "Alt's alt (<|>) should append containers"
  assert $ (l [1,2] <|> l [3,4]) == l [1,2,3,4]

  -- Applicative
  --   pure :: forall a. a -> f a
  log "Applicative's pure should construct a container with a single value"
  assert $ pure 5 == l [5]

  -- Apply
  --   apply :: forall a b. f (a -> b) -> f a -> f b
  -- Todo - pass in a helper container of functions
  -- or function that creates a container of functions

  -- Bind c
  --   bind :: forall a b. m a -> (a -> m b) -> m b
  log "Bind's bind (>>=) should append the results of a container-generating function\
  \applied to each element in the container"
  assert $ (l [1,2,3] >>= \x -> l [x,10+x]) == l [1,11,2,12,3,13]

  -- Eq
  --   eq :: a -> a -> Boolean
  log "Eq's eq (==) should correctly test containers for equality"
  assert $ l [1,2] == l [1,2]
  assert $ not $ l [1,2] == l [2,2]

  -- Eq1 -- missing from NonEmptyList, LazyNonEmptyList
  --   eq1 :: forall a. Eq a => f a -> f a -> Boolean
  -- Todo

  -- Extend
  --   extend :: forall b a. (w a -> b) -> w a -> w b
  log "Extend's extend (<<=) should create a container containing the results\
  \of a function that is applied to increasingly smaller chunks of an input\
  \container. Each iteration drops an element from the front of the input container."
  assert $ (sum <<= l [1,2,3,4]) == l [10,9,7,4]

  -- Foldable
  --   foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  --   foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  --   foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
  -- These are just the pre-existing tests. They could be more comprehensive.

  log "foldl should be stack-safe"
  void $ pure $ foldl (+) 0 k100

  log "foldMap should be stack-safe"
  void $ pure $ foldMap Additive k100

  log "foldMap should be left-to-right"
  assert $ foldMap show (rg 1 5) == "12345"

  -- FoldableWithIndex
  --   foldrWithIndex :: forall a b. (i -> a -> b -> b) -> b -> f a -> b
  --   foldlWithIndex :: forall a b. (i -> b -> a -> b) -> b -> f a -> b
  --   foldMapWithIndex :: forall a m. Monoid m => (i -> a -> m) -> f a -> m
  -- Todo - Existing tests, opportunities for improvement

  log "foldlWithIndex should be correct"
  assert $ foldlWithIndex (\i b _ -> i + b) 0 (rg 0 10000) == 50005000

  log "foldlWithIndex should be stack-safe"
  void $ pure $ foldlWithIndex (\i b _ -> i + b) 0 k100

  log "foldrWithIndex should be correct"
  assert $ foldrWithIndex (\i _ b -> i + b) 0 (rg 0 10000) == 50005000

  log "foldrWithIndex should be stack-safe"
  void $ pure $ foldrWithIndex (\i _ b -> i + b) 0 k100

  log "foldMapWithIndex should be stack-safe"
  void $ pure $ foldMapWithIndex (\i _ -> Additive i) k100

  log "foldMapWithIndex should be left-to-right"
  assert $ foldMapWithIndex (\i _ -> show i) (l [0, 0, 0]) == "012"

  -- Functor
  --   map :: forall a b. (a -> b) -> f a -> f b

  log "map should maintain order"
  assert $ rg 1 5 == (map identity $ rg 1 5)

  log "map should be stack-safe"
  void $ pure $ map identity k100
  -- Todo - The below test also performs the same stack-safety check

  log "map should be correct"
  assert $ rg 1 100000 == (map (_ + 1) $ rg 0 99999)


  -- FunctorWithIndex
  --   mapWithIndex :: forall a b. (i -> a -> b) -> f a -> f b
  -- Todo - improve pre-existing

  log "mapWithIndex should take a container of values and apply a function which also takes the index into account"
  assert $ mapWithIndex add (l [0, 1, 2, 3]) == l [0, 2, 4, 6]

  -- Monad
  --   indicates Applicative and Bind
  --   No specific tests

  -- Ord
  --   compare :: a -> a -> Ordering
  -- Todo - add tests

  -- Ord1 -- missing from NonEmptyList, LazyNonEmptyList
  --   compare1 :: forall a. Ord a => f a -> f a -> Ordering
  -- Todo - add tests

  -- Semigroup
  --   append :: a -> a -> a

  log "append should concatenate two containers"
  assert $ (l [1, 2]) <> (l [3, 4]) == (l [1, 2, 3, 4])

  log "append should be stack-safe"
  void $ pure $ k100 <> k100

  -- Show
  --   show :: a -> String
  -- This is not testable in a generic way

  -- Traversable
  --   traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
  --   sequence :: forall a m. Applicative m => t (m a) -> m (t a)
  -- Todo - improve pre-existing tests
  -- Todo - add sequence test

  log "traverse should be stack-safe"
  assert $ traverse Just k100 == Just k100

  -- TraversableWithIndex
  --   traverseWithIndex :: forall a b m. Applicative m => (i -> a -> m b) -> t a -> m (t b)

  log "traverseWithIndex should be stack-safe"
  assert $ traverseWithIndex (const Just) k100 == Just k100

  log "traverseWithIndex should be correct"
  assert $ traverseWithIndex (\i a -> Just $ i + a) (l [2, 2, 2])
           == Just (l [2, 3, 4])

  -- Unfoldable1
  --   unfoldr1 :: forall a b. (b -> Tuple a (Maybe b)) -> b -> t a

  let
    step1 :: Int -> Tuple Int (Maybe Int)
    step1 n = Tuple n (if n >= 5 then Nothing else Just (n + 1))

  log "unfoldr1 should maintain order"
  assert $ rg 1 5 == unfoldr1 step1 1

  -- ===========   Functions   ===========

  -- Todo - split
  -- log "catMaybe should take a container of Maybe values and throw out Nothings"
  -- assert $ catMaybes (l [Nothing, Just 2, Nothing, Just 4]) == l [2, 4]

  log "concat should join a container of containers"
  assert $ (concat (l [l [1, 2], l [3, 4]])) == l [1, 2, 3, 4]

  let
    doubleAndOrig :: Int -> c Int
    doubleAndOrig x = cons (x * 2) $ singleton x

  log "concatMap should be equivalent to (concat <<< map)"
  assert $ concatMap doubleAndOrig (l [1, 2, 3]) == concat (map doubleAndOrig (l [1, 2, 3]))

  log "cons should add an element to the front of the container"
  assert $ cons 1 (l [2, 3]) == l [1,2,3]

  log "elemIndex should return the index of an item that a predicate returns true for in a container"
  assert $ elemIndex 1 (l [1, 2, 1]) == Just 0
  assert $ elemIndex 4 (l [1, 2, 1]) == Nothing

  log "elemLastIndex should return the last index of an item in a container"
  assert $ elemLastIndex 1 (l [1, 2, 1]) == Just 2
  assert $ elemLastIndex 4 (l [1, 2, 1]) == Nothing

  -- Todo split
  -- log "filter should remove items that don't match a predicate"
  -- assert $ filter odd (range 0 10) == l [1, 3, 5, 7, 9]

  --log "filterM should remove items that don't match a predicate while using a monadic behaviour"
  --assert $ filterM (Just <<< odd) (range 0 10) == Just (l [1, 3, 5, 7, 9])
  --assert $ filterM (const Nothing) (rg 0 10) == Nothing

  log "findIndex should return the index of an item that a predicate returns true for in a container"
  assert $ findIndex (_ /= 1) (l [1, 2, 1]) == Just 1
  assert $ findIndex (_ == 3) (l [1, 2, 1]) == Nothing

  log "findLastIndex should return the last index of an item in a container"
  assert $ findLastIndex (_ /= 1) (l [2, 1, 2]) == Just 2
  assert $ findLastIndex (_ == 3) (l [2, 1, 2]) == Nothing

  log "foldM should perform a fold using a monadic step function"
  assert $ foldM (\x y -> Just (x + y)) 0 (rg 1 10) == Just 55
  assert $ foldM (\_ _ -> Nothing) 0 (rg 1 10) == Nothing

  log "index (!!) should return Just x when the index is within the bounds of the container"
  assert $ l [1, 2, 3] `index` 0 == (Just 1)
  assert $ l [1, 2, 3] `index` 1 == (Just 2)
  assert $ l [1, 2, 3] `index` 2 == (Just 3)

  log "index (!!) should return Nothing when the index is outside of the bounds of the container"
  assert $ l [1, 2, 3] `index` 6 == Nothing
  assert $ l [1, 2, 3] `index` (-1) == Nothing

  -- todo split
  -- log "insertAt should add an item at the specified index"
  -- assert $ (insertAt 0 1 (l [2, 3])) == Just (l [1, 2, 3])
  -- assert $ (insertAt 1 1 (l [2, 3])) == Just (l [2, 1, 3])
  -- assert $ (insertAt 2 1 (l [2, 3])) == Just (l [2, 3, 1])

  -- log "insertAt should return Nothing if the index is out of range"
  -- assert $ (insertAt 7 8 $ l [1,2,3]) == Nothing

  log "intersect should return the intersection of two containers"
  assert $ intersect (l [1, 2, 3, 4, 3, 2, 1]) (l [1, 1, 2, 3]) == l [1, 2, 3, 3, 2, 1]

  log "intersectBy should return the intersection of two containers using the specified equivalence relation"
  assert $ intersectBy (\x y -> (x * 2) == y) (l [1, 2, 3]) (l [2, 6]) == l [1, 3]

  log "length should return the number of items in a container"
  assert $ length (l [1]) == 1
  assert $ length (l [1, 2, 3, 4, 5]) == 5

  log "length should be stack-safe"
  void $ pure $ length k100

  -- todo split
  -- log "modifyAt should update an item at the specified index"
  -- assert $ (modifyAt 0 (_ + 1) (l [1, 2, 3])) == Just (l [2, 2, 3])
  -- assert $ (modifyAt 1 (_ + 1) (l [1, 2, 3])) == Just (l [1, 3, 3])

  -- log "modifyAt should return Nothing if the index is out of range"
  -- assert $ (modifyAt 7 (_ + 1) $ l [1,2,3]) == Nothing

  log "nubEq should remove duplicate elements from the container, keeping the first occurence"
  assert $ nubEq (l [1, 2, 2, 3, 4, 1]) == l [1, 2, 3, 4]

  log "nubByEq should remove duplicate items from the container using a supplied predicate"
  let mod3eq = eq `on` \n -> mod n 3
  assert $ nubByEq mod3eq (l [1, 3, 4, 5, 6]) == l [1, 3, 5]

  log "range should create an inclusive container of integers for the specified start and end"
  assert $ (range 3 3) == l [3]
  assert $ (range 0 5) == l [0, 1, 2, 3, 4, 5]
  assert $ (range 2 (-3)) == l [2, 1, 0, -1, -2, -3]

  log "reverse should reverse the order of items in a container"
  assert $ (reverse (l [1, 2, 3])) == l [3, 2, 1]

  log "singleton should construct a container with a single value"
  assert $ singleton 5 == l [5]

  log "snoc should add an item to the end of a container"
  assert $ l [1, 2, 3] `snoc` 4 == l [1, 2, 3, 4]

  -- Todo toUnfoldable

  log "union should produce the union of two containers"
  assert $ union (l [1, 2, 3]) (l [2, 3, 4]) == l [1, 2, 3, 4]
  assert $ union (l [1, 1, 2, 3]) (l [2, 3, 4]) == l [1, 1, 2, 3, 4]

  log "unionBy should produce the union of two containers using the specified equality relation"
  assert $ unionBy (\_ y -> y < 5) (l [1, 2, 3]) (l [2, 3, 4, 5, 6]) == l [1, 2, 3, 5, 6]

  log "unzip should deconstruct a container of tuples into a tuple of containers"
  assert $ unzip (l [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"]) == Tuple (l [1, 2, 3]) (l ["a", "b", "c"])

  log "zip should use the specified function to zip two containers together"
  assert $ zip (l [1, 2, 3]) (l ["a", "b", "c"]) == l [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"]

  log "zipWith should use the specified function to zip two containers together"
  assert $ zipWith (\x y -> l [show x, y]) (l [1, 2, 3]) (l ["a", "b", "c"]) == l [l ["1", "a"], l ["2", "b"], l ["3", "c"]]

  log "zipWithA should use the specified function to zip two containers together"
  assert $ zipWithA (\x y -> Just $ Tuple x y) (l [1, 2, 3]) (l ["a", "b", "c"]) == Just (l [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"])
