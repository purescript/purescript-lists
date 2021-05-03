module Test.AllTests where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, class Plus, empty)
import Control.Comonad (class Comonad)
import Control.Extend (class Extend, (<<=))
import Control.Lazy (class Lazy)
import Control.Monad.Rec.Class (class MonadRec)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Data.Array as Array
import Data.Eq (class Eq1, eq1)
import Data.Foldable (class Foldable, foldMap, foldl, sum, traverse_)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.Function (on)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Int (odd)
import Data.List as L
import Data.List.Lazy as LL
import Data.List.Lazy.NonEmpty as LNEL
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Monoid.Additive (Additive(..))
import Data.Ord (class Ord1, compare1)
import Data.Traversable (class Traversable, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Unfoldable1 (class Unfoldable1, unfoldr1)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Test.API (Common, CommonDiffEmptiability, OnlyCanEmpty, OnlyLazy, OnlyNonEmpty, OnlyStrict)
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

testCommon :: forall c.
  Alt c =>
  Applicative c =>
  Apply c =>
  Bind c =>
  Eq (c Int) =>
  Eq1 c =>
  Extend c =>
  Foldable c =>
  FoldableWithIndex Int c =>
  Functor c =>
  FunctorWithIndex Int c =>
  Monad c =>
  Ord (c Int) =>
  Ord1 c =>
  Semigroup (c Int) =>
  Show (c Int) =>
  Traversable c =>
  TraversableWithIndex Int c =>
  Unfoldable1 c =>
  --
  Eq (c String) =>
  Eq (c (Tuple Int String)) =>
  Eq (c (c String)) =>
  Eq (c (c Int)) => -- temp
  Eq (c (Array Int)) =>
  Show (c String) =>
  Show (c (Tuple Int String)) =>
  Show (c (c String)) =>
  Show (c (c Int)) => -- temp
  Show (c (Array Int)) =>
  Common c -> Effect Unit
testCommon
  r@{ makeCollection

  , concat
  , concatMap
  , cons
  , elemIndex
  , elemLastIndex
  , findIndex
  , findLastIndex
  , foldM
  , index
  , intersect
  , intersectBy
  , length
  , nubEq
  , nubByEq
  , range
  , reverse
  , singleton
  , snoc
  , toUnfoldable
  , union
  , unionBy
  , unzip
  , zip
  , zipWith
  , zipWithA

  , appendFoldable
  , insert
  , insertBy
  , nub
  , nubBy
  , some
  , someRec
  , sort
  , sortBy
  , transpose
  } = do
  let
    -- l :: forall f a. Foldable f => f a -> c a
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
  assertEqual { actual: makeCollection [mul 10, mul 100] <*> l [1, 2, 3], expected: l [10, 20, 30, 100, 200, 300] }
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
  --   Indicates Applicative and Bind, which are already tested above

  -- Ord
  --   compare :: a -> a -> Ordering
  log "compare should determine the ordering of two collections"
  assertEqual { actual: compare (l [1]) (l [1]), expected: EQ }
  assertEqual { actual: compare (l [0]) (l [1]), expected: LT }
  assertEqual { actual: compare (l [2]) (l [1]), expected: GT }
  assertEqual { actual: compare (l [1]) (l [1, 1]), expected: LT }
  assertEqual { actual: compare (l [1, 1]) (l [1]), expected: GT }
  assertEqual { actual: compare (l [1, 1]) (l [1, 2]), expected: LT }
  assertEqual { actual: compare (l [1, 2]) (l [1, 1]), expected: GT }

  -- Ord1
  --   compare1 :: forall a. Ord a => f a -> f a -> Ordering
  log "compare1 should determine the ordering of two collections"
  assertEqual { actual: compare1 (l [1]) (l [1]), expected: EQ }
  assertEqual { actual: compare1 (l [0]) (l [1]), expected: LT }
  assertEqual { actual: compare1 (l [2]) (l [1]), expected: GT }
  assertEqual { actual: compare1 (l [1]) (l [1, 1]), expected: LT }
  assertEqual { actual: compare1 (l [1, 1]) (l [1]), expected: GT }
  assertEqual { actual: compare1 (l [1, 1]) (l [1, 2]), expected: LT }
  assertEqual { actual: compare1 (l [1, 2]) (l [1, 1]), expected: GT }

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

  log "concat should join a collection of collections"
  assertEqual { actual: r.concat $ l [l [1, 2], l [3, 4]], expected: l [1, 2, 3, 4] }
  assertEqual { actual: r.concat $ l [l [1, 2], l [3, 4]], expected: l [1, 2, 3, 4] }

  let
    doubleAndOrig :: Int -> c Int
    doubleAndOrig x = cons (x * 2) $ singleton x

  log "concatMap should be equivalent to (concat <<< map)"
  assertEqual { actual: concatMap doubleAndOrig $ l [1, 2, 3], expected: r.concat $ map doubleAndOrig $ l [1, 2, 3] }

  log "cons should add an element to the front of the collection"
  assertEqual { actual: cons 1 $ l [2, 3], expected: l [1,2,3] }

  log "elemIndex should return the index of an item that a predicate returns true for in a collection"
  assertEqual { actual: elemIndex 1 $ l [1, 2, 1], expected: Just 0 }
  assertEqual { actual: elemIndex 4 $ l [1, 2, 1], expected: Nothing }

  log "elemLastIndex should return the last index of an item in a collection"
  assertEqual { actual: elemLastIndex 1 $ l [1, 2, 1], expected: Just 2 }
  assertEqual { actual: elemLastIndex 4 $ l [1, 2, 1], expected: Nothing }


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

  log "intersect should return the intersection of two collections"
  assertEqual { actual: intersect (l [1, 2, 3, 4, 3, 2, 1]) $ l [1, 1, 2, 3], expected: l [1, 2, 3, 3, 2, 1] }

  log "intersectBy should return the intersection of two collections using the specified equivalence relation"
  assertEqual { actual: intersectBy (\x y -> x * 2 == y) (l [1, 2, 3]) $ l [2, 6], expected: l [1, 3] }

  log "length should return the number of items in a collection"
  assertEqual { actual: r.length $ l [1], expected: 1 }
  assertEqual { actual: r.length $ l [1, 2, 3, 4, 5], expected: 5 }

  log "length should be stack-safe"
  void $ pure $ length bigCollection

  log "nubEq should remove duplicate elements from the collection, keeping the first occurence"
  assertEqual { actual: nubEq (l [1, 2, 2, 3, 4, 1]), expected: l [1, 2, 3, 4] }

  log "nubByEq should remove duplicate items from the collection using a supplied predicate"
  let mod3eq = eq `on` \n -> mod n 3
  assertEqual { actual: nubByEq mod3eq $ l [1, 3, 4, 5, 6], expected: l [1, 3, 5] }

  log "range should create an inclusive collection of integers for the specified start and end"
  assertEqual { actual: range 3 3, expected: l [3] }
  assertEqual { actual: range 0 5, expected: l [0, 1, 2, 3, 4, 5] }
  assertEqual { actual: range 2 (-3), expected: l [2, 1, 0, -1, -2, -3] }

  log "reverse should reverse the order of items in a collection"
  assertEqual { actual: r.reverse $ l [1, 2, 3], expected: l [3, 2, 1] }

  log "singleton should construct a collection with a single value"
  assertEqual { actual: singleton 5, expected: l [5] }

  log "snoc should add an item to the end of a collection"
  assertEqual { actual: l [1, 2, 3] `snoc` 4, expected: l [1, 2, 3, 4] }

  log "toUnfoldable should convert to any unfoldable collection"
  traverse_ (\xs -> assertEqual { actual: toUnfoldable (l xs), expected: xs })
    [ [1]
    , [1,2,3]
    , [4,0,0,1,25,36,458,5842,23757]
    ]

  log "union should produce the union of two collections"
  assertEqual { actual: union (l [1, 2, 3]) $ l [2, 3, 4], expected: l [1, 2, 3, 4] }
  assertEqual { actual: union (l [1, 1, 2, 3]) $ l [2, 3, 4], expected: l [1, 1, 2, 3, 4] }

  log "unionBy should produce the union of two collections using the specified equality relation"
  assertEqual { actual: unionBy (\_ y -> y < 5) (l [1, 2, 3]) $ l [2, 3, 4, 5, 6], expected: l [1, 2, 3, 5, 6] }

  log "unzip should deconstruct a collection of tuples into a tuple of collections"
  assertEqual { actual: r.unzip $ l [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"], expected: Tuple (l [1, 2, 3]) $ l ["a", "b", "c"] }

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

  log "appendFoldable should append a foldable collection to another collection"
  assertEqual { actual: appendFoldable (l [1, 2, 3]) [4, 5], expected: l [1, 2, 3, 4, 5] }

  log "insert should add an item at the appropriate place in a sorted list"
  assertEqual { actual: insert 2 $ l [1, 1, 3], expected: l [1, 1, 2, 3] }
  assertEqual { actual: insert 4 $ l [1, 2, 3], expected: l [1, 2, 3, 4] }
  assertEqual { actual: insert 0 $ l [1, 2, 3], expected: l [0, 1, 2, 3] }

  log "insertBy should add an item at the appropriate place in a sorted collection using the specified comparison"
  assertEqual { actual: insertBy (flip compare) 4 $ l [1, 2, 3], expected: l [4, 1, 2, 3] }
  assertEqual { actual: insertBy (flip compare) 0 $ l [1, 2, 3], expected: l [1, 2, 3, 0] }

  log "nub should remove duplicate elements from a collection, keeping the first occurrence"
  assertEqual { actual: nub (l [1, 2, 2, 3, 4, 1]), expected: l [1, 2, 3, 4] }

  log "nubBy should remove duplicate items from a collection using a supplied predicate"
  assertEqual { actual: nubBy (compare `on` Array.length) $ l [[1],[2],[3,4]] , expected: l [[1],[3,4]] }

  -- some :: forall f a. Alternative f => Lazy (f (c a)) => f a -> f (c a)
  -- someRec :: forall f a. MonadRec f => Alternative f => f a -> f (c a)

  log "sort should reorder a collection into ascending order based on the result of compare"
  assertEqual { actual: sort (l [1, 3, 2, 5, 6, 4]), expected: l [1, 2, 3, 4, 5, 6] }

  log "sortBy should reorder a collection into ascending order based on the result of a comparison function"
  assertEqual { actual: sortBy (flip compare) $ l [1, 3, 2, 5, 6, 4]
              , expected: l [6, 5, 4, 3, 2, 1] }

  log "transpose should swap 'rows' and 'columns' of a collection of collections"
  assertEqual { actual: transpose (l [l [1,2,3], l[4,5,6], l [7,8,9]])
              , expected: l [l [1,4,7], l[2,5,8], l [3,6,9]] }
  log "transpose should skip elements when row lengths don't match"
  assertEqual { actual: transpose (l [l [10, 11], l [20], l [30, 31, 32]])
              , expected: l [l [10, 20, 30], l [11, 31], l [32]] }

-- Todo - question:
{-
Should we have a specialized replicate, or just
reuse the one provided by Unfoldable?
-- If reusing from unfoldable, do we need to test here?
-}

  -- replicate :: forall a. Int -> a -> c a
  -- log "replicate should produce an list containing an item a specified number of times"
  -- assertEqual { actual: replicate 3 5, expected: l [5, 5, 5] }
  -- assert $ replicate 1 "foo" == l ["foo"]
  -- assert $ replicate 0 "foo" == l []
  -- assert $ replicate (-1) "foo" == l []

  {-
  log "unfoldable replicate should be stack-safe"
  -- even for strict lists? Possibly high memory consumption
  void $ pure $ length $ replicate 100000 1


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




testCommonDiffEmptiability :: forall c cInverse canEmpty nonEmpty cPattern.
  Eq (c (nonEmpty Int)) =>
  Eq (canEmpty Int) =>
  Eq (c (c Int)) =>
  Show (c (nonEmpty Int)) =>
  Show (canEmpty Int) =>
  Show (c (c Int)) =>
  SkipBroken ->
  CommonDiffEmptiability c cInverse canEmpty nonEmpty cPattern ->
  Effect Unit
testCommonDiffEmptiability skip
  { makeCollection
  , makeCanEmptyCollection
  , makeNonEmptyCollection

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
    -- l :: forall f a. Foldable f => f a -> c a
    l = makeCollection

    -- cel :: forall f a. Foldable f => f a -> canEmpty a
    -- cel = toCanEmpty <<< l
    -- cel x = toCanEmpty (makeCollection x)
    cel = makeCanEmptyCollection

    -- nel :: forall f a. Foldable f => f a -> nonEmpty a
    -- nel x = toNonEmpty (makeCollection x)
    nel = makeNonEmptyCollection

    assertSkip :: Array SkipBroken -> (_ -> Boolean) -> Effect Unit
    assertSkip = assertSkipHelper skip

  printTestType "Common (where signatures differ based on emptiability)"

  log "catMaybes should take a collection of Maybe values and remove the Nothings"
  assertEqual { actual: catMaybes (l [Nothing, Just 2, Nothing, Just 4]), expected: cel [2, 4] }

  log "drop should remove the specified number of items from the front of an list"
  assertEqual { actual: (drop 1 (l [1, 2, 3])), expected: cel [2, 3] }
  assertEqual { actual: (drop (-1) (l [1, 2, 3])), expected: cel [1, 2, 3] }

  log "dropWhile should remove all values that match a predicate from the front of an list"
  assertEqual { actual: (dropWhile (_ /= 1) (l [1, 2, 3])), expected: cel [1, 2, 3] }
  assertEqual { actual: (dropWhile (_ /= 2) (l [1, 2, 3])), expected: cel [2, 3] }
  --assert $ (dropWhile (_ /= 1) nil) == nil

  -- Surprised this does not work with $
  -- let l10 = l $ Array.range 0 10
  let l10 = l (Array.range 0 10)

  log "filter should remove items that don't match a predicate"
  assertEqual { actual: filter odd l10, expected: cel [1, 3, 5, 7, 9] }

  log "filterM should remove items that don't match a predicate while using a monadic behaviour"
  assertEqual { actual: filterM (Just <<< odd) l10, expected: Just $ cel [1, 3, 5, 7, 9] }
  assertEqual { actual: filterM (const Nothing) l10, expected: Nothing }


  log "group should group consecutive equal elements into lists"
  assertEqual { actual: group (l [1, 2, 2, 3, 3, 3, 1]), expected: l [nel [1], nel [2, 2], nel [3, 3, 3], nel [1]] }

  log "groupAll should group equal elements into lists"
  assertSkip [SkipBrokenLazyCanEmpty]
   \_ -> groupAll (l [1, 2, 2, 3, 3, 3, 1]) == l [nel [1, 1], nel [2, 2], nel [3, 3, 3]]
  --assert $ groupAll (l [1, 2, 2, 3, 3, 3, 1]) == l [nel [1, 1], nel [2, 2], nel [3, 3, 3]]

  log "groupBy should group consecutive equal elements into lists based on an equivalence relation"
  assertEqual { actual: groupBy (eq `on` (_ `mod` 10)) (l [1, 2, 12, 3, 13, 23, 11]), expected: l [nel [1], nel [2, 12], nel [3, 13, 23], nel [11]] }

  -- todo - wait for this to be reworked
  -- log "groupAllBy should group equal elements into lists based on an comparison function"
  --assertEqual { actual: groupAllBy (compare `on` mod 10) (l [1, 2, 12, 3, 13, 23, 11]), expected: l [nel [1, 11], nel [2, 12], nel [3, 13, 23]] }

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
  assert $ (take 0 (l [1, 2])) == cel []
  assert $ (take (-1) (l [1, 2])) == cel []

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




testOnlyCanEmpty :: forall c nonEmpty.
  Alternative c =>
  MonadPlus c =>
  MonadZero c =>
  Monoid (c Int) =>
  Plus c =>
  Unfoldable c =>
  --
  Eq (c Int) =>
  Eq (c (nonEmpty Int)) =>
  OnlyCanEmpty c nonEmpty -> Effect Unit
testOnlyCanEmpty
  { makeCollection
  , makeNonEmptyCollection

  , fromFoldable
  , head
  , init
  , last
  , tail
  , uncons

  , null
  , many
  , manyRec
  } = do
  let
    l = makeCollection
    nel = makeNonEmptyCollection

    nil :: c Int
    nil = l []

  printTestType "Only canEmpty"

  -- ======= Typeclass tests ========

  -- Alternative
  -- applicative and plus
  -- (f <|> g) <*> x == (f <*> x) <|> (g <*> x)
  -- empty <*> f == empty

  -- MonadPlus
  -- Additional law on MonadZero
  -- (x <|> y) >>= f == (x >>= f) <|> (y >>= f)

  -- MonadZero
  -- monad and alternative
  -- empty >>= f = empty

  -- Monoid
  -- mempty :: c
  log "mempty should not change the collection it is appended to"
  assert $ l [5] <> mempty == l [5]
  log "mempty should be an empty collection"
  assert $ l [] == (mempty :: c Int)

  -- Plus
  -- empty :: forall a. c a
  log "empty should create an empty collection"
  assert $ l [] == (empty :: c Int)

  -- Unfoldable
  -- unfoldr :: forall a b. (b -> Maybe (Tuple a b)) -> b -> c a

  log "unfoldr should maintain order"
  let
    step :: Int -> Maybe (Tuple Int Int)
    step 6 = Nothing
    step n = Just (Tuple n (n + 1))
  assert $ l [1, 2, 3, 4, 5] == unfoldr step 1


  -- ======= Functions tests ========

  --fromFoldable :: forall f. Foldable f => f ~> c
  --already extensively checked in common tests

  -- These are the remaining functions that can't be deduplicated due to use of Maybe

  -- Todo - double-check the phrasing on these? Might be confusing to refer to a
  -- non-empty canEmpty list.

  log "head should return a Just-NEL.NonEmptyListped first value of a non-empty list"
  assert $ head (l [1, 2]) == Just 1

  log "head should return Nothing for an empty list"
  assert $ head nil == Nothing

  -- Todo - phrasing should be changed to note all but last (not all but first).
  log "init should return a Just-NEL.NonEmptyListped list containing all the items in an list apart from the first for a non-empty list"
  assert $ init (l [1, 2, 3]) == Just (l [1, 2])

  log "init should return Nothing for an empty list"
  assert $ init nil == Nothing


  log "last should return a Just-NEL.NonEmptyListped last value of a non-empty list"
  assert $ last (l [1, 2]) == Just 2

  log "last should return Nothing for an empty list"
  assert $ last nil == Nothing


  log "tail should return a Just-NEL.NonEmptyListped list containing all the items in an list apart from the first for a non-empty list"
  assert $ tail (l [1, 2, 3]) == Just (l [2, 3])

  log "tail should return Nothing for an empty list"
  assert $ tail nil == Nothing


  log "uncons should return nothing when used on an empty list"
  assert $ isNothing (uncons nil)

  log "uncons should split an list into a head and tail record when there is at least one item"
  assert $ uncons (l [1]) == Just {head: 1, tail: l []}
  assert $ uncons (l [1, 2, 3]) == Just {head: 1, tail: l [2, 3]}





testOnlyNonEmpty :: forall c canEmpty.
  Comonad c =>
  --, Foldable1 c => -- missing from LazyNonEmptyList
  --, Traversable1 c => -- missing from LazyNonEmptyList
  Eq (c Int) =>
  Eq (canEmpty Int) =>
  Show (c Int) =>
  Show (canEmpty Int) =>
  OnlyNonEmpty c canEmpty -> Effect Unit
testOnlyNonEmpty
  r@{ makeCollection
  , makeCanEmptyCollection

  , fromFoldable
  , head
  , init
  , last
  , tail
  , uncons

  , fromList
  , toList

  -- ? toUnfoldable1?

  } = do
  let
    l = makeCollection

    cel = makeCanEmptyCollection

  printTestType "Only nonEmpty"

  -- ======= Typeclass tests ========

  -- Todo

  -- Comonad
  -- Foldable1
  -- Traversable1

  -- ======= Functions tests ========

  log "fromList should convert from a List to a NonEmptyList"
  assertEqual { actual: r.fromList $ cel [1, 2, 3], expected: Just $ l [1, 2, 3] }
  assertEqual { actual: r.fromList $ cel ([] :: _ Int), expected: Nothing }

  log "toList should convert from a NonEmptyList to a List"
  assertEqual { actual: r.toList $ l [1, 2, 3], expected: cel [1, 2, 3] }


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





testOnlyLazy :: forall c.
  -- Lazy (c Int) => -- missing from LazyNonEmptyList
  --
  Eq (c Int) =>
  OnlyLazy c -> Effect Unit
testOnlyLazy
  { makeCollection

  , alterAt
  , insertAt
  , modifyAt
  , updateAt

  , iterate
  , repeat
  , cycle
  , foldrLazy
  , scanlLazy
  } = do
  let
    l = makeCollection

  printTestType "Only Lazy"

  log "insertAt should add an item at the specified index"
  assert $ (insertAt 0 1 (l [2, 3])) == (l [1, 2, 3])
  assert $ (insertAt 1 1 (l [2, 3])) == (l [2, 1, 3])
  assert $ (insertAt 2 1 (l [2, 3])) == (l [2, 3, 1])

  log "modifyAt should update an item at the specified index"
  assert $ (modifyAt 0 (_ + 1) (l [1, 2, 3])) == (l [2, 2, 3])
  assert $ (modifyAt 1 (_ + 1) (l [1, 2, 3])) == (l [1, 3, 3])

  log "updateAt should replace an item at the specified index"
  assert $ (updateAt 0 9 (l [1, 2, 3])) == (l [9, 2, 3])
  assert $ (updateAt 1 9 (l [1, 2, 3])) == (l [1, 9, 3])


testOnlyStrict :: forall c.
  Eq (c Int) =>
  OnlyStrict c -> Effect Unit
testOnlyStrict
  { makeCollection

  , alterAt
  , insertAt
  , modifyAt
  , updateAt
  } = do

  let
    l = makeCollection

  printTestType "Only Strict"

  -- log "insertAt should add an item at the specified index"
  -- assertEqual { actual: insertAt 0 1 $ l [2, 3], expected: Just $ l [1, 2, 3] }
  -- assertEqual { actual: insertAt 1 1 $ l [2, 3], expected: Just $ l [2, 1, 3] }
  -- assertEqual { actual: insertAt 2 1 $ l [2, 3], expected: Just $ l [2, 3, 1] }

  -- log "insertAt should return Nothing if the index is out of range"
  -- assertEqual { actual: insertAt 7 8 $ l [1,2,3], expected: Nothing }

  -- todo insertAt test
  -- missing from original test suite

  -- log "modifyAt should update an item at the specified index"
  -- assertEqual { actual: modifyAt 0 (_ + 1) $ l [1, 2, 3], expected: Just $ l [2, 2, 3] }
  -- assertEqual { actual: modifyAt 1 (_ + 1) $ l [1, 2, 3], expected: Just $ l [1, 3, 3] }

  -- log "modifyAt should return Nothing if the index is out of range"
  -- assertEqual { actual: modifyAt 7 (_ + 1) $ l [1,2,3], expected: Nothing }

  -- todo modifyAt test
  -- missing from original test suite

  log "updateAt should replace an item at the specified index"
  assert $ (updateAt 0 9 (l [1, 2, 3])) == Just (l [9, 2, 3])
  assert $ (updateAt 1 9 (l [1, 2, 3])) == Just (l [1, 9, 3])

  log "updateAt should return Nothing if the index is out of range"
  assert $ (updateAt 5 9 (l [1, 2, 3])) == Nothing



-- Functions that cannot be tested generically.

-- Debating whether these should be passed a record defined in the API?


assertSkipAlways :: (_ -> Boolean) -> Effect Unit
assertSkipAlways _ =
  log "...skipped"

testOnlyStrictCanEmpty :: Effect Unit
testOnlyStrictCanEmpty = do

  let
    l :: forall f a. Foldable f => f a -> L.List a
    l = L.fromFoldable

  printTestType "Only Strict canEmpty"

  -- Common function names, but different signatures

  log "deleteAt should remove an item at the specified index"
  assert $ L.deleteAt 0 (l [1, 2, 3]) == Just (l [2, 3])
  assert $ L.deleteAt 1 (l [1, 2, 3]) == Just (l [1, 3])

  -- Corner Cases

  -- Unique functions


testOnlyStrictNonEmpty :: Effect Unit
testOnlyStrictNonEmpty = do

  let
    l :: forall f a. Foldable f => f a -> NEL.NonEmptyList a
    l = unsafePartial fromJust <<< NEL.fromFoldable

    cel :: forall f a. Foldable f => f a -> L.List a
    cel = L.fromFoldable

  printTestType "Only Strict NonEmpty"

  -- Common function names, but different signatures

  log "deleteAt should remove an item at the specified index"
  assertSkipAlways \_ -> NEL.deleteAt 0 (l [1, 2, 3]) == Just (cel [2, 3])
  assertSkipAlways \_ -> NEL.deleteAt 1 (l [1, 2, 3]) == Just (cel [1, 3])

  -- Corner Cases

  -- Unique functions


testOnlyLazyCanEmpty :: Effect Unit
testOnlyLazyCanEmpty = do

  let
    l :: forall f a. Foldable f => f a -> LL.List a
    l = LL.fromFoldable

  printTestType "Only Lazy canEmpty"

  -- Common function names, but different signatures

  log "deleteAt should remove an item at the specified index"
  assert $ LL.deleteAt 0 (l [1, 2, 3]) == l [2, 3]
  assert $ LL.deleteAt 1 (l [1, 2, 3]) == l [1, 3]

  -- Corner Cases

  -- Unique functions

  -- replicate  (specialized from Unfoldable's replicate)
  -- replicateM (specialized from Unfoldable's replicateA)


testOnlyLazyNonEmpty :: Effect Unit
testOnlyLazyNonEmpty = do

  let
    l :: forall f a. Foldable f => f a -> LNEL.NonEmptyList a
    l = unsafePartial fromJust <<< LNEL.fromFoldable

    cel :: forall f a. Foldable f => f a -> LL.List a
    cel = LL.fromFoldable

  printTestType "Only Lazy NonEmpty"

  -- Common function names, but different signatures

  log "deleteAt should remove an item at the specified index"
  assert $ LNEL.deleteAt 0 (l [1, 2, 3]) == cel [2, 3]
  assert $ LNEL.deleteAt 1 (l [1, 2, 3]) == cel [1, 3]

  -- Corner Cases

  -- Unique functions

  -- replicate1  (specialized from Unfoldable1's replicate1)
  -- replicate1M (specialized from Unfoldable1's replicate1A)


