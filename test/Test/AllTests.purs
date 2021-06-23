module Test.AllTests where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, class Plus, empty)
import Control.Comonad (class Comonad)
import Control.Extend (class Extend, (<<=))
import Control.Lazy (class Lazy, defer)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Data.Array as Array
import Data.Eq (class Eq1, eq1)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, sum, traverse_)
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
import Data.Ord (class Ord1, compare1)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Unfoldable as Unfoldable
import Data.Unfoldable1 (class Unfoldable1, unfoldr1)
import Data.Unfoldable1 as Unfoldable1
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Test.API (Common, CommonDiffEmptiability, OnlyCanEmpty, OnlyLazy, OnlyLazyCanEmpty, OnlyNonEmpty, OnlyStrict, OnlyStrictCanEmpty, OnlyStrictNonEmpty, OnlyLazyNonEmpty)
import Test.Assert (assertEqual, assertEqual', assertFalse, assertTrue)

{-
Todos

Improve typeclass law checking
-}

{-
This "Skip" code is temporarily being used during development.
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

type AssertRec a = { actual :: a , expected :: a }

assertSkipHelper :: forall a. Eq a => Show a =>
  SkipBroken -> Array SkipBroken -> (Unit -> AssertRec a) -> Effect Unit
assertSkipHelper skip arr f =
  case Array.elem skip arr of
    true -> log "...skipped"
    false -> assertEqual $ f unit

assertSkipAlways :: forall a. (Unit -> AssertRec a) -> Effect Unit
assertSkipAlways _ =
  log "...skipped"

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
  -- The below contraints are for unit testing asserts:
  Eq (c String) =>
  Eq (c (Tuple Int String)) =>
  Eq (c (c String)) =>
  Eq (c (c Int)) =>
  Eq (c (Array Int)) =>
  Show (c String) =>
  Show (c (Tuple Int String)) =>
  Show (c (c String)) =>
  Show (c (c Int)) =>
  Show (c (Array Int)) =>
  -- parameters:
  Common c ->
  Effect Unit
testCommon
  { makeCollection

  , appendFoldable
  , concat
  , concatMap
  , cons
  , elemIndex
  , elemLastIndex
  , findIndex
  , findLastIndex
  , foldM
  , index
  , insert
  , insertBy
  , intersect
  , intersectBy
  , length
  , nub
  , nubBy
  , nubEq
  , nubByEq
  , range
  , reverse
  , singleton
  , snoc
  , some
  , someRec
  , sort
  , sortBy
  , toUnfoldable
  , transpose
  , union
  , unionBy
  , unzip
  , zip
  , zipWith
  , zipWithA

  } = do
  let
    l = makeCollection

    rg :: Int -> Int -> c Int
    rg = range

    bigCollection :: c _
    bigCollection = range 1 100000

  printTestType "Common"

  -- Testing range asap, since many other tests rely on it.
  log "range should create an inclusive collection of integers for the specified start and end"
  assertEqual { actual: range 3 3, expected: l [3] }
  assertEqual { actual: range 0 5, expected: l [0, 1, 2, 3, 4, 5] }
  assertEqual { actual: range 2 (-3), expected: l [2, 1, 0, -1, -2, -3] }

  -- ======= Typeclass tests ========

  -- Alt
  --   alt :: forall a. f a -> f a -> f a

  log "Alt's alt (<|>) should append collections"
  assertEqual { actual: l [1,2] <|> l [3,4], expected: l [1,2,3,4] }

  -- Applicative
  --   pure :: forall a. a -> f a

  log "Applicative's pure should construct a collection with a single value"
  assertEqual { actual: pure 5, expected: l [5] }

  -- Apply
  --   apply :: forall a b. f (a -> b) -> f a -> f b

  -- Todo - Fix ordering mismatch between list types. Also ensure ordering is the same for arrays.

  log "Apply's apply (<*>) should have cartesian product behavior for non-zippy collections"
  assertEqual { actual: l [mul 10, mul 100] <*> l [1, 2, 3], expected: l [10, 20, 30, 100, 200, 300] }

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
  --   foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  --   foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  --   foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m

  log "Foldable's foldl should correctly fold left-to-right"
  assertEqual { actual: foldl (\b a -> b * 10 + a) 0 $ rg 1 5, expected: 12345 }

  log "Foldable's foldr should correctly fold right-to-left"
  assertEqual { actual: foldr (\a b -> b * 10 + a) 0 $ rg 1 5, expected: 54321 }

  log "Foldable's foldMap should be left-to-right"
  assertEqual { actual: foldMap show $ rg 1 5, expected: "12345" }

  log "Foldable's foldl should be stack-safe"
  void $ pure $ foldl (+) 0 bigCollection

  log "Foldable's foldr should be stack-safe"
  void $ pure $ foldr (+) 0 bigCollection

  log "Foldable's foldMap should be stack-safe"
  void $ pure $ foldMap Additive bigCollection

  -- FoldableWithIndex
  --   foldlWithIndex :: forall a b. (i -> b -> a -> b) -> b -> f a -> b
  --   foldrWithIndex :: forall a b. (i -> a -> b -> b) -> b -> f a -> b
  --   foldMapWithIndex :: forall a m. Monoid m => (i -> a -> m) -> f a -> m

  log "FoldableWithIndex's foldlWithIndex should correctly fold left-to-right"
  assertEqual { actual: foldlWithIndex (\_ b a -> b * 10 + a) 0 $ rg 1 5, expected: 12345 }

  log "FoldableWithIndex's foldrWithIndex should correctly fold right-to-left"
  assertEqual { actual: foldrWithIndex (\_ a b -> b * 10 + a) 0 $ rg 1 5, expected: 54321 }

  log "FoldableWithIndex's foldMapWithIndex should be left-to-right"
  assertEqual { actual: foldMapWithIndex (\_ a -> show a) $ rg 1 5, expected: "12345" }


  log "FoldableWithIndex's foldlWithIndex should increment indices"
  assertEqual { actual: foldlWithIndex (\i b _ -> b * 10 + i) 0 $ l [0, 0, 0, 0, 0], expected: 1234 }

  log "FoldableWithIndex's foldrWithIndex should decrement indices"
  assertEqual { actual: foldrWithIndex (\i _ b -> b * 10 + i) 0 $ l [0, 0, 0, 0, 0], expected: 43210 }

  log "FoldableWithIndex's foldMapWithIndex should increment indices"
  assertEqual { actual: foldMapWithIndex (\i _ -> show i) $ l [0, 0, 0, 0, 0], expected: "01234" }


  log "FoldableWithIndex's foldlWithIndex should be stack-safe"
  void $ pure $ foldlWithIndex (\i b _ -> i + b) 0 bigCollection

  log "FoldableWithIndex's foldrWithIndex should be stack-safe"
  void $ pure $ foldrWithIndex (\i _ b -> i + b) 0 bigCollection

  log "FoldableWithIndex's foldMapWithIndex should be stack-safe"
  void $ pure $ foldMapWithIndex (\i _ -> Additive i) bigCollection

  -- Functor
  --   map :: forall a b. (a -> b) -> f a -> f b

  log "Functor's map should be correct"
  assertEqual { actual: map (add 1) $ rg 0 4, expected: rg 1 5 }

  log "Functor's map should be stack-safe"
  void $ pure $ map identity bigCollection

  -- FunctorWithIndex
  --   mapWithIndex :: forall a b. (i -> a -> b) -> f a -> f b

  log "FunctorWithIndex's mapWithIndex should take a collection of values and apply a function which also takes the index into account"
  assertEqual { actual: mapWithIndex add $ l [10, 10, 10, 10, 10], expected: l [10, 11, 12, 13, 14] }

  log "FunctorWithIndex's mapWithIndex should be stack-safe"
  void $ pure $ mapWithIndex add bigCollection

  -- Monad
  --   Indicates Applicative and Bind, which are already tested above

  -- Ord
  --   compare :: a -> a -> Ordering

  log "Ord's compare should determine the ordering of two collections"
  assertEqual { actual: compare (l [1]) (l [1]), expected: EQ }
  assertEqual { actual: compare (l [0]) (l [1]), expected: LT }
  assertEqual { actual: compare (l [2]) (l [1]), expected: GT }
  assertEqual { actual: compare (l [1]) (l [1, 1]), expected: LT }
  assertEqual { actual: compare (l [1, 1]) (l [1]), expected: GT }
  assertEqual { actual: compare (l [1, 1]) (l [1, 2]), expected: LT }
  assertEqual { actual: compare (l [1, 2]) (l [1, 1]), expected: GT }

  -- Ord1
  --   compare1 :: forall a. Ord a => f a -> f a -> Ordering

  log "Ord1's compare1 should determine the ordering of two collections"
  assertEqual { actual: compare1 (l [1]) (l [1]), expected: EQ }
  assertEqual { actual: compare1 (l [0]) (l [1]), expected: LT }
  assertEqual { actual: compare1 (l [2]) (l [1]), expected: GT }
  assertEqual { actual: compare1 (l [1]) (l [1, 1]), expected: LT }
  assertEqual { actual: compare1 (l [1, 1]) (l [1]), expected: GT }
  assertEqual { actual: compare1 (l [1, 1]) (l [1, 2]), expected: LT }
  assertEqual { actual: compare1 (l [1, 2]) (l [1, 1]), expected: GT }

  -- Semigroup
  --   append :: a -> a -> a

  log "Semigroup's append (<>) should concatenate two collections"
  assertEqual { actual: l [1, 2] <> l [3, 4], expected: l [1, 2, 3, 4] }

  log "Semigroup's append (<>) should be stack-safe"
  void $ pure $ bigCollection <> bigCollection

  -- Show
  --   show :: a -> String
  -- This is not testable in a generic way

  -- Traversable
  --   traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
  --   sequence :: forall a m. Applicative m => t (m a) -> m (t a)

  let
    safeDiv :: Int -> Int -> Maybe Int
    safeDiv _ 0 = Nothing
    safeDiv n d = Just $ n / d

  log "Traversable's traverse should be correct"
  assertEqual { actual: traverse (safeDiv 12) $ l [1, 2, 3, 4], expected: Just $ l [12, 6, 4, 3] }
  assertEqual { actual: traverse (safeDiv 12) $ l [1, 2, 0, 4], expected: Nothing }

  log "Traversable's sequence should be correct"
  assertEqual { actual: sequence $ l [Just 1, Just 2, Just 3], expected: Just $ l [1, 2, 3] }
  assertEqual { actual: sequence $ l [Just 1, Nothing, Just 3], expected: Nothing }

  log "Traversable's traverse should be stack-safe"
  assertEqual { actual: traverse Just bigCollection, expected: Just bigCollection }

  log "Traversable's sequence should be stack-safe"
  assertEqual { actual: sequence $ map Just bigCollection, expected: Just bigCollection }

  -- TraversableWithIndex
  --   traverseWithIndex :: forall a b m. Applicative m => (i -> a -> m b) -> t a -> m (t b)

  log "TraversableWithIndex's traverseWithIndex should be correct"
  assertEqual { actual: traverseWithIndex safeDiv $ l [2, 2, 2, 2], expected: Just $ l [0, 0, 1, 1] }
  assertEqual { actual: traverseWithIndex safeDiv $ l [2, 2, 0, 2], expected: Nothing }

  log "TraversableWithIndex's traverseWithIndex should be stack-safe"
  assertEqual { actual: traverseWithIndex (const Just) bigCollection, expected: Just bigCollection }

  -- Unfoldable1
  --   unfoldr1 :: forall a b. (b -> Tuple a (Maybe b)) -> b -> t a

  let
    step1 :: Int -> Tuple Int (Maybe Int)
    step1 n = Tuple n $ if n >= 5 then Nothing else Just $ n + 1

  log "Unfoldable1's unfoldr1 should maintain order"
  assertEqual { actual: unfoldr1 step1 1, expected: rg 1 5 }

  log "Unfoldable1's replicate1 should be correct"
  assertEqual { actual: Unfoldable1.replicate1 3 1, expected: l [1, 1, 1] }
  assertEqual { actual: Unfoldable1.replicate1 1 1, expected: l [1] }
  assertEqual { actual: Unfoldable1.replicate1 0 1, expected: l [1] }
  assertEqual { actual: Unfoldable1.replicate1 (-1) 1, expected: l [1] }

  log "Unfoldable1's replicate1 should be stack-safe"
  assertEqual { actual: length (Unfoldable1.replicate1 100000 1), expected: 100000 }

  -- ===========   Functions   ===========

  log "appendFoldable should append a foldable collection to another collection"
  assertEqual { actual: appendFoldable (l [1, 2, 3]) [4, 5], expected: l [1, 2, 3, 4, 5] }
  assertEqual { actual: appendFoldable (l [1, 2, 3]) [], expected: l [1, 2, 3] }

  log "concat should join a collection of collections"
  assertEqual { actual: concat (l [l [1, 2], l [3, 4]]), expected: l [1, 2, 3, 4] }
  assertEqual { actual: concat (l [l [1, 2]]), expected: l [1, 2] }

  let
    doubleAndOrig :: Int -> c Int
    doubleAndOrig x = cons (x * 2) $ singleton x

  log "concatMap should be equivalent to (concat <<< map)"
  assertEqual { actual: concatMap doubleAndOrig $ l [1, 2, 3], expected: concat (map doubleAndOrig $ l [1, 2, 3]) }
  assertEqual { actual: concatMap doubleAndOrig $ l [1, 2, 3], expected: l [2, 1, 4, 2, 6, 3] }

  log "cons should add an element to the front of the collection"
  assertEqual { actual: cons 1 $ l [2, 3], expected: l [1,2,3] }

  log "elemIndex should return the index of an item that a predicate returns true for in a collection"
  assertEqual { actual: elemIndex 1 $ l [1, 2, 1], expected: Just 0 }
  assertEqual { actual: elemIndex 4 $ l [1, 2, 1], expected: Nothing }
  assertEqual { actual: elemIndex (-1) $ l [1, 2, 1], expected: Nothing }

  log "elemLastIndex should return the last index of an item in a collection"
  assertEqual { actual: elemLastIndex 1 $ l [1, 2, 1], expected: Just 2 }
  assertEqual { actual: elemLastIndex 4 $ l [1, 2, 1], expected: Nothing }
  assertEqual { actual: elemLastIndex (-1) $ l [1, 2, 1], expected: Nothing }

  log "findIndex should return the index of an item that a predicate returns true for in a collection"
  assertEqual { actual: findIndex (_ /= 1) $ l [1, 2, 1], expected: Just 1 }
  assertEqual { actual: findIndex (_ == 3) $ l [1, 2, 1], expected: Nothing }

  log "findLastIndex should return the last index of an item in a collection"
  assertEqual { actual: findLastIndex (_ /= 1) $ l [2, 1, 2], expected: Just 2 }
  assertEqual { actual: findLastIndex (_ == 3) $ l [2, 1, 2], expected: Nothing }

  log "foldM should perform a fold using a monadic step function"
  let
    foldMFunc _ 0 = Nothing
    foldMFunc x y = Just $ x + y
  assertEqual { actual: foldM foldMFunc 0 $ l [1, 2, 3, 4], expected: Just 10 }
  assertEqual { actual: foldM foldMFunc 0 $ l [1, 2, 0, 4], expected: Nothing }

  log "index (!!) should return Just x when the index is within the bounds of the collection"
  assertEqual { actual: l [1, 2, 3] `index` 0, expected: Just 1 }
  assertEqual { actual: l [1, 2, 3] `index` 1, expected: Just 2 }
  assertEqual { actual: l [1, 2, 3] `index` 2, expected: Just 3 }

  log "index (!!) should return Nothing when the index is outside of the bounds of the collection"
  assertEqual { actual: l [1, 2, 3] `index` 6, expected: Nothing }
  assertEqual { actual: l [1, 2, 3] `index` (-1), expected: Nothing }

  log "insert should add an item at the appropriate place in a sorted collection"
  assertEqual { actual: insert 2 $ l [1, 1, 3], expected: l [1, 1, 2, 3] }
  assertEqual { actual: insert 4 $ l [1, 2, 3], expected: l [1, 2, 3, 4] }
  assertEqual { actual: insert 0 $ l [1, 2, 3], expected: l [0, 1, 2, 3] }

  log "insertBy should add an item at the appropriate place in a sorted collection using the specified comparison"
  assertEqual { actual: insertBy (flip compare) 4 $ l [1, 2, 3], expected: l [4, 1, 2, 3] }
  assertEqual { actual: insertBy (flip compare) 0 $ l [1, 2, 3], expected: l [1, 2, 3, 0] }

  log "intersect should return the intersection of two collections"
  assertEqual { actual: intersect (l [1, 2, 3, 4, 3, 2, 1]) $ l [1, 1, 2, 3], expected: l [1, 2, 3, 3, 2, 1] }

  log "intersectBy should return the intersection of two collections using the specified equivalence relation"
  assertEqual { actual: intersectBy (\x y -> x * 2 == y) (l [1, 2, 3]) $ l [2, 6], expected: l [1, 3] }

  log "length should return the number of items in a collection"
  assertEqual { actual: length (l [1]), expected: 1 }
  assertEqual { actual: length (l [1, 2, 3, 4, 5]), expected: 5 }

  log "length should be stack-safe"
  void $ pure $ length bigCollection

  log "nub should remove duplicate elements from a collection, keeping the first occurrence"
  assertEqual { actual: nub (l [1, 2, 2, 3, 4, 1]), expected: l [1, 2, 3, 4] }

  log "nubBy should remove duplicate items from a collection using a supplied predicate"
  assertEqual { actual: nubBy (compare `on` Array.length) $ l [[1],[2],[3,4]], expected: l [[1],[3,4]] }

  log "nubEq should remove duplicate elements from the collection, keeping the first occurence"
  assertEqual { actual: nubEq (l [1, 2, 2, 3, 4, 1]), expected: l [1, 2, 3, 4] }

  log "nubByEq should remove duplicate items from the collection using a supplied predicate"
  let mod3eq = eq `on` \n -> mod n 3
  assertEqual { actual: nubByEq mod3eq $ l [1, 3, 4, 5, 6], expected: l [1, 3, 5] }

  log "reverse should reverse the order of items in a collection"
  assertEqual { actual: reverse (l [1, 2, 3]), expected: l [3, 2, 1] }

  log "singleton should construct a collection with a single value"
  assertEqual { actual: singleton 5, expected: l [5] }

  log "snoc should add an item to the end of a collection"
  assertEqual { actual: l [1, 2, 3] `snoc` 4, expected: l [1, 2, 3, 4] }

  -- Todo - create tests for these functions

  -- some :: forall f a. Alternative f => Lazy (f (c a)) => f a -> f (c a)
  -- someRec :: forall f a. MonadRec f => Alternative f => f a -> f (c a)

  log "sort should reorder a collection into ascending order based on the result of compare"
  assertEqual { actual: sort (l [1, 3, 2, 5, 6, 4]), expected: l [1, 2, 3, 4, 5, 6] }

  log "sortBy should reorder a collection into ascending order based on the result of a comparison function"
  assertEqual { actual: sortBy (flip compare) $ l [1, 3, 2, 5, 6, 4]
              , expected: l [6, 5, 4, 3, 2, 1] }

  log "toUnfoldable should convert to any unfoldable collection"
  traverse_ (\xs -> assertEqual { actual: toUnfoldable (l xs), expected: xs })
    [ [1]
    , [1,2,3]
    , [4,0,0,1,25,36,458,5842,23757]
    ]

  log "transpose should swap 'rows' and 'columns' of a collection of collections"
  assertEqual { actual: transpose (l [l [1,2,3], l[4,5,6], l [7,8,9]])
              , expected: l [l [1,4,7], l[2,5,8], l [3,6,9]] }
  log "transpose should skip elements when row lengths don't match"
  assertEqual { actual: transpose (l [l [10, 11], l [20], l [30, 31, 32]])
              , expected: l [l [10, 20, 30], l [11, 31], l [32]] }

  log "union should produce the union of two collections"
  assertEqual { actual: union (l [1, 2, 3]) $ l [2, 3, 4], expected: l [1, 2, 3, 4] }
  assertEqual { actual: union (l [0, 0]) $ l [1, 1], expected: l [0, 0, 1] }

  log "unionBy should produce the union of two collections using the specified equality relation"
  assertEqual { actual: unionBy mod3eq (l [1, 5, 1, 2]) $ l [3, 4, 3, 3], expected: l [1, 5, 1, 2, 3] }

  log "unzip should deconstruct a collection of tuples into a tuple of collections"
  assertEqual { actual: unzip (l [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"]), expected: Tuple (l [1, 2, 3]) $ l ["a", "b", "c"] }

  log "zip should use the specified function to zip two collections together"
  assertEqual { actual: zip (l [1, 2, 3]) $ l ["a", "b", "c"], expected: l [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"] }
  assertEqual { actual: zip (l [1, 2, 3]) $ l ["a", "b"], expected: l [Tuple 1 "a", Tuple 2 "b"] }
  assertEqual { actual: zip (l [1, 2]) $ l ["a", "b", "c"], expected: l [Tuple 1 "a", Tuple 2 "b"] }

  log "zipWith should use the specified function to zip two collections together"
  assertEqual { actual: zipWith (\x y -> l [show x, y]) (l [1, 2, 3]) $ l ["a", "b", "c"], expected: l [l ["1", "a"], l ["2", "b"], l ["3", "c"]] }
  assertEqual { actual: zipWith (\x y -> l [show x, y]) (l [1, 2, 3]) $ l ["a", "b"], expected: l [l ["1", "a"], l ["2", "b"]] }
  assertEqual { actual: zipWith (\x y -> l [show x, y]) (l [1, 2]) $ l ["a", "b", "c"], expected: l [l ["1", "a"], l ["2", "b"]] }

  log "zipWithA should use the specified function to zip two collections together"
  let
    zipWithAFunc 0 _ = Nothing
    zipWithAFunc x y = Just $ Tuple x y
  assertEqual { actual: zipWithA zipWithAFunc (l [1, 2, 3]) $ l ["a", "b", "c"], expected: Just $ l [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"] }
  assertEqual { actual: zipWithA zipWithAFunc (l [1, 2, 0]) $ l ["a", "b", "c"], expected: Nothing }
  assertEqual { actual: zipWithA zipWithAFunc (l [1, 2, 3]) $ l ["a", "b"], expected: Just $ l [Tuple 1 "a", Tuple 2 "b"] }
  assertEqual { actual: zipWithA zipWithAFunc (l [1, 2, 0]) $ l ["a", "b"], expected: Just $ l [Tuple 1 "a", Tuple 2 "b"] }
  assertEqual { actual: zipWithA zipWithAFunc (l [1, 2]) $ l ["a", "b", "c"], expected: Just $ l [Tuple 1 "a", Tuple 2 "b"] }
  assertEqual { actual: zipWithA zipWithAFunc (l [0, 2]) $ l ["a", "b", "c"], expected: Nothing }


testCommonDiffEmptiability :: forall c cInverse canEmpty nonEmpty cPattern.
  Eq (c (nonEmpty Int)) =>
  Eq (canEmpty Int) =>
  Eq (c (c Int)) =>
  Eq (c Int) =>
  Show (c (nonEmpty Int)) =>
  Show (canEmpty Int) =>
  Show (c (c Int)) =>
  Show (c Int) =>
  -- parameters:
  SkipBroken ->
  CommonDiffEmptiability c cInverse canEmpty nonEmpty cPattern ->
  Effect Unit
testCommonDiffEmptiability skip
  { makeCollection
  , makeCanEmptyCollection
  , makeNonEmptyCollection
  , makeInverseCollection

  , catMaybes
  , cons'
  , delete
  , deleteBy
  , difference
  , dropEnd
  , drop
  , dropWhile
  , filter
  , filterM
  , group
  , groupAll
  , groupAllBy
  , groupBy
  , mapMaybe
  , partition
  , pattern
  , slice
  , snoc'
  , span
  , stripPrefix
  , take
  , takeEnd
  , takeWhile

  } = do
  let
    l = makeCollection
    cel = makeCanEmptyCollection
    nel = makeNonEmptyCollection
    ivl = makeInverseCollection

    assertSkip :: forall a. Eq a => Show a => Array SkipBroken -> (_ -> AssertRec a) -> Effect Unit
    assertSkip = assertSkipHelper skip

  printTestType "Common (where signatures differ based on emptiability)"

  log "catMaybes should take a collection of Maybe values and remove the Nothings"
  assertEqual { actual: catMaybes (l [Nothing, Just 2, Nothing, Just 4]), expected: cel [2, 4] }

  log "cons' should create a collection by prepending an element to an 'inverse' (can-empty or not) collection"
  assertEqual { actual: cons' 1 $ ivl [2, 3], expected: l [1, 2, 3] }

  log "delete should remove the first matching item from a collection"
  assertEqual { actual: delete 1 $ l [1, 2, 1], expected: cel [2, 1] }
  assertEqual { actual: delete 2 $ l [1, 2, 1], expected: cel [1, 1] }
  assertEqual { actual: delete 3 $ l [1, 2, 1], expected: cel [1, 2, 1] }
  assertEqual { actual: delete 2 $ l [2], expected: cel [] }

  log "deleteBy should remove the first equality-relation-matching item from a collection"
  assertEqual { actual: deleteBy (/=) 2 $ l [1, 2, 1], expected: cel [2, 1] }
  assertEqual { actual: deleteBy (/=) 1 $ l [1, 2, 1], expected: cel [1, 1] }
  assertEqual { actual: deleteBy (/=) 1 $ l [1, 1, 1], expected: cel [1, 1, 1] }
  assertEqual { actual: deleteBy (/=) 1 $ l [2], expected: cel [] }

  log "difference (\\\\) should return the 'difference' between two collections"
  assertEqual { actual: l [1, 2, 3, 4, 3, 2] `difference` l [1], expected: cel [2,3,4,3,2] }
  assertEqual { actual: l [1, 2, 3, 4, 3, 2] `difference` l [2], expected: cel [1,3,4,3,2] }
  assertEqual { actual: l [1, 2, 3, 4, 3, 2] `difference` l [2, 2], expected: cel [1,3,4,3] }
  assertEqual { actual: l [1, 2, 3, 4, 3, 2] `difference` l [2, 2, 2], expected: cel [1,3,4,3] }
  assertEqual { actual: l [1, 2, 3] `difference` l [3, 1, 2], expected: cel [] }

  log "drop should remove the specified number of items from the front of a collection"
  assertEqual { actual: drop 1 $ l [1, 2, 3], expected: cel [2, 3] }
  assertEqual { actual: drop (-1) $ l [1, 2, 3], expected: cel [1, 2, 3] }

  log "dropEnd should remove the specified number of items from the end of a collection"
  assertEqual { actual: dropEnd (-1) $ l [1, 2, 3], expected: cel [1, 2, 3] }
  assertEqual { actual: dropEnd 0 $ l [1, 2, 3], expected: cel [1, 2, 3] }
  assertEqual { actual: dropEnd 1 $ l [1, 2, 3], expected: cel [1, 2] }
  assertEqual { actual: dropEnd 2 $ l [1, 2, 3], expected: cel [1] }
  assertEqual { actual: dropEnd 3 $ l [1, 2, 3], expected: cel [] }
  assertEqual { actual: dropEnd 4 $ l [1, 2, 3], expected: cel [] }

  log "dropWhile should remove all values that match a predicate from the front of a collection"
  assertEqual { actual: dropWhile (_ /= 1) $ l [1, 2, 3], expected: cel [1, 2, 3] }
  assertEqual { actual: dropWhile (_ /= 2) $ l [1, 2, 3], expected: cel [2, 3] }

  -- Surprised this does not work with $
  -- let l10 = l $ Array.range 0 10
  let l10 = l (Array.range 0 10)
  -- More discussion here:
  -- https://discourse.purescript.org/t/apply-is-not-always-a-valid-substitute-for-parens/2301

  log "filter should remove items that don't match a predicate"
  assertEqual { actual: filter odd l10, expected: cel [1, 3, 5, 7, 9] }

  log "filterM should remove items that don't match a predicate while using a monadic behaviour"
  assertEqual { actual: filterM (Just <<< odd) l10, expected: Just $ cel [1, 3, 5, 7, 9] }
  assertEqual { actual: filterM (const Nothing) l10, expected: Nothing }

  log "group should group consecutive equal elements into collections"
  assertEqual { actual: group (l [1, 2, 2, 3, 3, 3, 1])
              , expected: l [nel [1], nel [2, 2], nel [3, 3, 3], nel [1]] }

  log "groupAll should group equal elements into collections"
  assertSkip [SkipBrokenLazyCanEmpty]
   \_ -> { actual: groupAll (l [1, 2, 2, 3, 3, 3, 1]), expected: l [nel [1, 1], nel [2, 2], nel [3, 3, 3]] }

  log "groupAllBy should sort then group equal elements into lists based on a comparison function"
  assertEqual { actual: groupAllBy (compare `on` (_ `div` 10)) $ l [32, 31, 21, 22, 11, 33]
              , expected: l [nel [11], nel [21, 22], nel [32, 31, 33]] }

  log "groupBy should group consecutive equal elements into collections based on an equivalence relation"
  assertEqual { actual: groupBy (eq `on` (_ `mod` 10)) $ l [1, 2, 12, 3, 13, 23, 11]
              , expected: l [nel [1], nel [2, 12], nel [3, 13, 23], nel [11]] }

  log "mapMaybe should transform every item in a collection, throwing out Nothing values"
  assertEqual { actual: mapMaybe (\x -> if x /= 0 then Just x else Nothing) $ l [0, 1, 0, 0, 2, 3]
              , expected: cel [1, 2, 3] }

  log "partition should separate a collection into a tuple of collections that do and do not satisfy a predicate"
  assertEqual { actual: partition (_ > 2) $ l [1, 5, 3, 2, 4]
              , expected: { yes: cel [5, 3, 4], no: cel [1, 2] } }


  log "slice should extract a sub-collection by an inclusive start and exclusive end index"
  let nums = l [0, 1, 2, 3, 4]
  assertEqual { actual: slice 1 3 nums, expected: cel [1, 2] }

  log "slice should make best effort for out-of-bounds (but intersecting) indices"
  assertEqual { actual: slice 3 7 nums, expected: cel [3, 4] }
  -- assertEqual { actual: slice (-2) 3 nums, expected: cel [1, 2] } -- Todo - broken, returns full collection

  log "slice should return an empty collection if indices do not intersect with available elements"
  assertEqual { actual: slice 5 7 nums, expected: cel [] }

  log "slice should return an empty collection if indices are not incrementing"
  assertEqual { actual: slice 3 1 nums, expected: cel [] }


  log "snoc' should create a collection by appending an element to an 'inverse' (can-empty or not) collection"
  assertEqual { actual: snoc' (ivl [1, 2]) 3, expected: l [1, 2, 3] }

  log "span should split a collection in two based on a predicate"
  assertEqual { actual: span (_ < 4) $ l [1, 2, 3, 4, 5, 6, 1]
              , expected: { init: cel [1, 2, 3], rest: cel [4, 5, 6, 1] } }
  assertEqual { actual: span (_ < 4) $ l [9, 2, 3, 4, 5, 6, 1]
              , expected: { init: cel [], rest: cel [9, 2, 3, 4, 5, 6, 1] } }

  log "stripPrefix should remove elements matching a pattern from the start of a collection"
  assertEqual { actual: stripPrefix (pattern (l [4, 2])) $ l [4, 2, 5, 1] , expected: Just $ cel [5, 1] }
  assertEqual { actual: stripPrefix (pattern (l [4, 2])) $ l [4, 2] , expected: Just $ cel [] }

  log "stripPrefix should return nothing if starting elements do not match pattern"
  assertEqual { actual: stripPrefix (pattern (l [4, 2])) $ l [4, 4, 2, 5, 1] , expected: Nothing }
  assertEqual { actual: stripPrefix (pattern (l [4, 2])) $ l [4] , expected: Nothing }

  log "take should keep the specified number of items from the front of a collection, discarding the rest"
  assertEqual { actual: take 1 $ l [1, 2, 3], expected: cel [1] }
  assertEqual { actual: take 2 $ l [1, 2, 3], expected: cel [1, 2] }
  assertEqual { actual: take 0 $ l [1, 2], expected: cel [] }
  assertEqual { actual: take (-1) $ l [1, 2], expected: cel [] }

  log "takeEnd should keep the specified number of items from the end of a collection, discarding the rest"
  assertSkip [SkipBrokenLazyCanEmpty]
    \_ -> { actual: takeEnd 1 $ l [1, 2, 3], expected: cel [3] }
  assertSkip [SkipBrokenLazyCanEmpty]
    \_ -> { actual: takeEnd 2 $ l [1, 2, 3], expected: cel [2, 3] }
  assertSkip [SkipBrokenLazyCanEmpty]
    \_ -> { actual: takeEnd 2 $ l [1], expected: cel [1] }

  log "takeWhile should keep all values that match a predicate from the front of a collection"
  assertEqual { actual: takeWhile (_ /= 2) $ l [1, 2, 3], expected: cel [1] }
  assertEqual { actual: takeWhile (_ /= 3) $ l [1, 2, 3], expected: cel [1, 2] }


testOnlyCanEmpty :: forall c nonEmpty cPattern.
  -- OnlyCanEmpty API
  Alternative c =>
  MonadPlus c =>
  MonadZero c =>
  Monoid (c Int) =>
  Plus c =>
  Unfoldable c =>
  -- Common API with additional canEmpty tests
  Alt c =>
  Apply c =>
  Bind c =>
  Extend c =>
  Foldable c =>
  FoldableWithIndex Int c =>
  Functor c =>
  FunctorWithIndex Int c =>
  Monad c =>
  Ord (c Int) =>
  Ord1 c =>
  Semigroup (c Int) =>
  Traversable c =>
  TraversableWithIndex Int c =>
  -- Constraints for unit test asserts
  Eq (c Int) =>
  Eq (c (nonEmpty Int)) =>
  Eq (c (c Int)) =>
  Eq (c (c String)) =>
  Eq (c (Array Int)) =>
  Eq (c (Tuple Int String)) =>
  Show (c Int) =>
  Show (c (nonEmpty Int)) =>
  Show (c (c Int)) =>
  Show (c (c String)) =>
  Show (c (Array Int)) =>
  Show (c (Tuple Int String)) =>
  -- parameters:
  SkipBroken ->
  OnlyCanEmpty c ->
  Common c ->
  CommonDiffEmptiability c nonEmpty c nonEmpty cPattern ->
  Effect Unit
testOnlyCanEmpty skip
  -- OnlyCanEmpty
  { makeCollection

  -- Only available for CanEmpty collections
  , null
  , many
  , manyRec

  -- Todo - can these be deduplicated into diff-empty using Maybe / identity constructor?

  -- Can't be deduplicated from NonEmpty collections due to use of Maybe
  , fromFoldable
  , head
  , init
  , last
  , tail
  , uncons
  }

  -- Test these common functions again with empty collections:

  -- Common
  { appendFoldable
  , concat
  , concatMap
  , cons
  , elemIndex
  , elemLastIndex
  , findIndex
  , findLastIndex
  , foldM
  , index
  , insert
  , insertBy
  , intersect
  , intersectBy
  , length
  , nub
  , nubBy
  , nubEq
  , nubByEq
  , reverse
  , singleton
  , snoc
  , some
  , someRec
  , sort
  , sortBy
  , toUnfoldable
  , transpose
  , union
  , unionBy
  , unzip
  , zip
  , zipWith
  , zipWithA
  }

  -- CommonDiffEmptiability
  { catMaybes
  -- , cons' -- tested in onlyNonEmpty
  , delete
  , deleteBy
  , difference
  , dropEnd
  , drop
  , dropWhile
  , filter
  , filterM
  , group
  , groupAll
  , groupAllBy
  , groupBy
  , mapMaybe
  , partition
  , pattern
  , slice
  -- , snoc' -- tested in onlyNonEmpty
  , span
  , stripPrefix
  , take
  , takeEnd
  , takeWhile
  } = do
  let
    l = makeCollection

    nil :: c Int
    nil = l []

    assertSkip :: forall a. Eq a => Show a => Array SkipBroken -> (_ -> AssertRec a) -> Effect Unit
    assertSkip = assertSkipHelper skip

  printTestType "Only canEmpty"

  -- ======= Typeclass tests ========

  -- Alternative
  -- applicative and plus
  -- (f <|> g) <*> x == (f <*> x) <|> (g <*> x)
  -- empty <*> x == empty

  log "Alternative's laws should be upheld"
  do -- limit scope for helper variables
    let
      f = l [mul 10]
      g = l [mul 100]
      x = l [1, 2, 3]

    -- Todo - likely broken for some list types until Apply is fixed

    assertEqual { actual: (f <|> g) <*> x, expected: (f <*> x) <|> (g <*> x) }
    assertEqual { actual: (f <|> g) <*> x, expected: l [10, 20, 30, 100, 200, 300] }
    assertEqual { actual: empty <*> x, expected: empty :: c Int }

  do -- limit scope for helper variable
    let
      f x = l [x, 10 + x]

    -- MonadZero
    -- monad and alternative
    -- empty >>= f = empty
    log "MonadZero's law should be upheld"
    assertEqual { actual: empty >>= f, expected: empty :: c Int }

    -- MonadPlus
    -- Additional law on MonadZero
    -- (x <|> y) >>= f == (x >>= f) <|> (y >>= f)
    log "MonadPlus's law should be upheld"
    let
      x = l [1, 2]
      y = l [3, 4]
    assertEqual { actual: (x <|> y) >>= f, expected: (x >>= f) <|> (y >>= f) }
    assertEqual { actual: (x <|> y) >>= f, expected: l [1,11,2,12,3,13,4,14] }

  -- Monoid
  -- mempty :: c

  log "Monoid's mempty should be an empty collection"
  assertEqual { actual: mempty :: c Int, expected: nil }

  log "Monoid's mempty should not change the collection it is appended to"
  do -- limit scope for helper variable
    let x = l [1, 2, 3]
    assertEqual { actual: x <> mempty, expected: x }
    assertEqual { actual: mempty <> x, expected: x }

  -- Plus
  -- empty :: forall a. c a

  log "Plus's empty should be an empty collection"
  assertEqual { actual: empty :: c Int, expected: nil }

  log "Plus's empty should not change the collection it is `alt`-ed (concatenated) with"
  do -- limit scope for helper variable
    let x = l [1, 2, 3]
    assertEqual { actual: x <|> empty, expected: x }
    assertEqual { actual: empty <|> x, expected: x }

  log "Plus's empty should remain unchanged when mapped over"
  assertEqual { actual: (add 1) <$> empty, expected: empty :: c Int }


  -- Unfoldable
  -- unfoldr :: forall a b. (b -> Maybe (Tuple a b)) -> b -> c a

  let
    step :: Int -> Maybe (Tuple Int Int)
    step n = if n > 5 then Nothing else Just $ Tuple n $ n + 1

  log "Unfoldable's unfoldr should maintain order"
  assertEqual { actual: unfoldr step 1, expected: l [1, 2, 3, 4, 5] }

  log "Unfoldable's replicate should be correct"
  assertEqual { actual: Unfoldable.replicate 3 1, expected: l [1, 1, 1] }
  assertEqual { actual: Unfoldable.replicate 1 1, expected: l [1] }
  assertEqual { actual: Unfoldable.replicate 0 1, expected: nil }
  assertEqual { actual: Unfoldable.replicate (-1) 1, expected: nil }

  log "Unfoldable's replicate should be stack-safe"
  assertEqual { actual: last (Unfoldable.replicate 100000 1), expected: Just 1 }

  -- ======= Functions tests ========

  -- These functions are not available for non-empty collections

  log "null should return true if collection is empty"
  assertTrue $ null nil
  assertFalse $ null (l [1])
  assertFalse $ null (l [1, 2])

  -- Todo - tests for these functions
  -- many :: forall f a. Alternative f => Lazy (f (c a)) => f a -> f (c a)
  -- manyRec :: forall f a. MonadRec f => Alternative f => f a -> f (c a)


  -- These are the remaining functions that can't be deduplicated due to use of Maybe

  log "fromFoldable should create a collection from another foldable collection"
  assertEqual { actual: fromFoldable [1, 2], expected: l [1, 2] }
  assertEqual { actual: fromFoldable [], expected: nil }
  assertEqual { actual: fromFoldable (Just 1), expected: l [1] }
  assertEqual { actual: fromFoldable Nothing, expected: nil }

  -- Todo - Is this good phrasing? Might be confusing to refer to a
  -- non-empty canEmpty list.

  log "head should return the first item of a non-empty collection"
  assertEqual { actual: head (l [1, 2]), expected: Just 1 }

  log "head should return Nothing for an empty collection"
  assertEqual { actual: head nil, expected: Nothing }


  log "init should drop the last item of a non-empty collection"
  assertEqual { actual: init (l [1, 2, 3]), expected: Just $ l [1, 2] }

  log "init should return Nothing for an empty collection"
  assertEqual { actual: init nil, expected: Nothing }


  log "last should return the last item of a non-empty collection"
  assertEqual { actual: last (l [1, 2]), expected: Just 2 }

  log "last should return Nothing for an empty collection"
  assertEqual { actual: last nil, expected: Nothing }


  log "tail should drop the first item of a non-empty collection"
  assertEqual { actual: tail (l [1, 2, 3]), expected: Just $ l [2, 3] }

  log "tail should return Nothing for an empty collection"
  assertEqual { actual: tail nil, expected: Nothing }


  log "uncons should split a non-empty collection into a head and tail record"
  assertEqual { actual: uncons (l [1]), expected: Just { head: 1, tail: nil } }
  assertEqual { actual: uncons (l [1, 2, 3]), expected: Just { head: 1, tail: l [2, 3] } }

  log "uncons should return nothing for an empty collection"
  assertEqual { actual: uncons nil, expected: Nothing }


  -- ========== Common API with additional canEmpty tests ==========

  log "Ensure common functions work with empty collections"

  -- ===== Common Typeclasses =====

  -- Alt
  --   alt :: forall a. f a -> f a -> f a
  log "Alt's alt (<|>) should work with empty collections"
  assertEqual { actual: l [1,2] <|> nil, expected: l [1,2] }
  assertEqual { actual: nil <|> l [3,4], expected: l [3,4] }
  assertEqual { actual: nil <|> nil, expected: nil }

  -- Apply
  --   apply :: forall a b. f (a -> b) -> f a -> f b
  log "Apply's apply (<*>) should work with empty collections"
  assertEqual { actual: l [] <*> l [1, 2, 3], expected: nil }
  assertEqual { actual: l [mul 10, mul 100] <*> nil, expected: nil }

  -- Bind c
  --   bind :: forall a b. m a -> (a -> m b) -> m b
  log "Bind's bind (>>=) should work with empty collections"
  assertEqual { actual: nil >>= \x -> l [x,10+x], expected: nil }
  assertEqual { actual: l [1,2,3] >>= \_ -> nil, expected: nil }

  -- Extend
  --   extend :: forall b a. (w a -> b) -> w a -> w b
  log "Extend's extend (<<=) should work with empty collections"
  assertEqual { actual: sum <<= nil, expected: nil }

  -- Foldable
  --   foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  --   foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  --   foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m

  log "Foldable's foldl should work with empty collections"
  assertEqual { actual: foldl (\b a -> b * 10 + a) 0 nil, expected: 0 }

  log "Foldable's foldr should work with empty collections"
  assertEqual { actual: foldr (\a b -> b * 10 + a) 0 nil, expected: 0 }

  log "Foldable's foldMap should work with empty collections"
  assertEqual { actual: foldMap show nil, expected: "" }

  -- FoldableWithIndex
  --   foldrWithIndex :: forall a b. (i -> a -> b -> b) -> b -> f a -> b
  --   foldlWithIndex :: forall a b. (i -> b -> a -> b) -> b -> f a -> b
  --   foldMapWithIndex :: forall a m. Monoid m => (i -> a -> m) -> f a -> m

  log "FoldableWithIndex's foldlWithIndex should work with empty collections"
  assertEqual { actual: foldlWithIndex (\_ b a -> b * 10 + a) 0 nil, expected: 0 }

  log "FoldableWithIndex's foldrWithIndex should work with empty collections"
  assertEqual { actual: foldrWithIndex (\_ a b -> b * 10 + a) 0 nil, expected: 0 }

  log "FoldableWithIndex's foldMapWithIndex should work with empty collections"
  assertEqual { actual: foldMapWithIndex (\_ a -> show a) nil, expected: "" }

  -- Functor
  --   map :: forall a b. (a -> b) -> f a -> f b
  log "Functor's map should work with empty collections"
  assertEqual { actual: map (add 1) nil, expected: nil }

  -- FunctorWithIndex
  --   mapWithIndex :: forall a b. (i -> a -> b) -> f a -> f b

  log "FunctorWithIndex's mapWithIndex should work with empty collections"
  assertEqual { actual: mapWithIndex add nil, expected: nil }

  -- Ord
  --   compare :: a -> a -> Ordering
  log "Ord's compare should work with empty collections"
  assertEqual { actual: compare nil nil, expected: EQ }
  assertEqual { actual: compare nil (l [1]), expected: LT }
  assertEqual { actual: compare (l [1]) nil, expected: GT }

  -- Ord1
  --   compare1 :: forall a. Ord a => f a -> f a -> Ordering
  log "Ord1's compare1 should work with empty collections"
  assertEqual { actual: compare1 nil nil, expected: EQ }
  assertEqual { actual: compare1 nil (l [1]), expected: LT }
  assertEqual { actual: compare1 (l [1]) nil, expected: GT }

  -- Semigroup
  --   append :: a -> a -> a

  log "Semigroup's append (<>) should work with empty collections"
  assertEqual { actual: l [1,2] <> nil, expected: l [1,2] }
  assertEqual { actual: nil <> l [3,4], expected: l [3,4] }
  assertEqual { actual: nil <> nil, expected: nil }

  -- Traversable
  --   traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
  --   sequence :: forall a m. Applicative m => t (m a) -> m (t a)

  let
    safeDiv :: Int -> Int -> Maybe Int
    safeDiv _ 0 = Nothing
    safeDiv n d = Just $ n / d

  log "Traversable's traverse should work with empty collections"
  assertEqual { actual: traverse (safeDiv 12) nil, expected: Just nil }

  log "Traversable's sequence should work with empty collections"
  assertEqual { actual: sequence (l []), expected: Just nil }

  -- TraversableWithIndex
  --   traverseWithIndex :: forall a b m. Applicative m => (i -> a -> m b) -> t a -> m (t b)

  log "TraversableWithIndex's traverseWithIndex should work with empty collections"
  assertEqual { actual: traverseWithIndex safeDiv nil, expected: Just nil }


  -- ===== Common Functions =====

  log "appendFoldable should work with empty collections"
  assertEqual { actual: appendFoldable nil [4, 5], expected: l [4, 5] }
  assertEqual { actual: appendFoldable nil [], expected: nil }

  log "concat should work with empty collections"
  assertEqual { actual: concat (l []), expected: nil }
  assertEqual { actual: concat (l [nil]), expected: nil }
  assertEqual { actual: concat (l [nil, l [1, 2], nil, l [3, 4], nil]), expected: l [1, 2, 3, 4] }

  let
    doubleAndOrig :: Int -> c Int
    doubleAndOrig x = cons (x * 2) $ singleton x

  log "concatMap should work with empty collections"
  assertEqual { actual: concatMap doubleAndOrig nil, expected: nil }

  log "cons should work with empty collections"
  assertEqual { actual: cons 1 nil, expected: l [1] }

  log "elemIndex should work with empty collections"
  assertEqual { actual: elemIndex 1 nil, expected: Nothing }

  log "elemLastIndex should work with empty collections"
  assertEqual { actual: elemLastIndex 1 nil, expected: Nothing }

  log "findIndex should work with empty collections"
  assertEqual { actual: findIndex (_ /= 1) nil, expected: Nothing }

  log "findLastIndex should work with empty collections"
  assertEqual { actual: findLastIndex (_ /= 1) nil, expected: Nothing }

  log "foldM should work with empty collections"
  assertEqual { actual: foldM (\x y -> Just $ x + y) 0 nil, expected: Just 0 }

  log "index (!!) should work with empty collections"
  assertEqual { actual: nil `index` 0, expected: Nothing }

  log "insert should work with empty collections"
  assertEqual { actual: insert 2 nil, expected: l [2] }

  log "insertBy should work with empty collections"
  assertEqual { actual: insertBy (flip compare) 4 nil, expected: l [4] }

  log "intersect should work with empty collections"
  assertEqual { actual: intersect nil $ l [1, 1, 2, 3], expected: nil }
  assertEqual { actual: intersect (l [1, 2, 3, 4, 3, 2, 1]) nil, expected: nil }
  assertEqual { actual: intersect nil nil, expected: nil }

  log "intersectBy should work with empty collections"
  assertEqual { actual: intersectBy (\x y -> x * 2 == y) nil $ l [2, 6], expected: nil }
  assertEqual { actual: intersectBy (\x y -> x * 2 == y) (l [1, 2, 3]) nil, expected: nil }
  assertEqual { actual: intersectBy (\x y -> x * 2 == y) nil nil, expected: nil }

  log "length should work with empty collections"
  assertEqual { actual: length nil, expected: 0 }

  log "nub should work with empty collections"
  assertEqual { actual: nub nil, expected: nil }

  log "nubBy should work with empty collections"
  assertEqual { actual: nubBy (compare `on` Array.length) $ l [], expected: l [] :: _ (_ Int) }

  log "nubEq should work with empty collections"
  assertEqual { actual: nubEq nil, expected: nil }

  log "nubByEq should work with empty collections"
  let mod3eq = eq `on` \n -> mod n 3
  assertEqual { actual: nubByEq mod3eq nil, expected: nil }

  log "reverse should work with empty collections"
  assertEqual { actual: reverse nil, expected: nil }

  log "snoc should work with empty collections"
  assertEqual { actual: nil `snoc` 4, expected: l [4] }

  -- Todo - create tests for these functions

  -- some :: forall f a. Alternative f => Lazy (f (c a)) => f a -> f (c a)
  -- someRec :: forall f a. MonadRec f => Alternative f => f a -> f (c a)

  log "sort should work with empty collections"
  assertEqual { actual: sort nil, expected: nil }

  log "sortBy should work with empty collections"
  assertEqual { actual: sortBy (flip compare) nil, expected: nil }

  log "toUnfoldable should work with empty collections"
  assertEqual { actual: toUnfoldable nil, expected: [] }

  log "transpose should work with empty collections"
  assertEqual { actual: transpose (l [l [10, 11], nil, l [30, 31, 32]])
              , expected: l [l [10, 30], l [11, 31], l [32]] }
  assertEqual { actual: transpose (l [] :: _ (_ Int)), expected: l []}

  log "union should work with empty collections"
  assertEqual { actual: union nil $ l [1, 1], expected: l [1] }
  assertEqual { actual: union (l [0, 0]) nil, expected: l [0, 0] }
  assertEqual { actual: union nil nil, expected: nil }

  log "unionBy should work with empty collections"
  assertEqual { actual: unionBy mod3eq nil $ l [3, 4, 3, 3], expected: l [3, 4] }
  assertEqual { actual: unionBy mod3eq (l [1, 5, 1, 2]) nil, expected: l [1, 5, 1, 2] }
  assertEqual { actual: unionBy mod3eq nil nil, expected: nil }

  log "unzip should work with empty collections"
  assertEqual { actual: unzip (l []), expected: Tuple nil nil }

  log "zip should work with empty collections"
  assertEqual { actual: zip nil $ l ["a", "b", "c"], expected: l [] }
  assertEqual { actual: zip (l [1, 2, 3]) (l [] :: _ String), expected: l [] }
  assertEqual { actual: zip nil (l [] :: _ String), expected: l []}

  log "zipWith should work with empty collections"
  assertEqual { actual: zipWith (\x y -> l [show x, y]) nil $ l ["a", "b", "c"], expected: l [] }
  assertEqual { actual: zipWith (\x y -> l [show x, y]) (l [1, 2, 3]) $ l [], expected: l [] }
  assertEqual { actual: zipWith (\x y -> l [show x, y]) nil $ l [], expected: l [] }

  log "zipWithA should work with empty collections"
  let
    zipWithAFunc 0 _ = Nothing
    zipWithAFunc x y = Just $ Tuple x y
  assertEqual { actual: zipWithA zipWithAFunc nil $ l ["a", "b", "c"], expected: Just $ l [] }
  assertEqual { actual: zipWithA zipWithAFunc (l [1, 2, 3]) $ l [], expected: Just $ l [] }
  assertEqual { actual: zipWithA zipWithAFunc (l [1, 2, 0]) $ l [], expected: Just $ l [] }
  assertEqual { actual: zipWithA zipWithAFunc nil $ l [], expected: Just $ l [] }

  -- ===== CommonDiffEmptiability Functions =====

  log "catMaybes should work with empty collections"
  assertEqual { actual: catMaybes (l []), expected: nil }

  log "delete should work with empty collections"
  assertEqual { actual: delete 3 nil, expected: nil }

  log "deleteBy should work with empty collections"
  assertEqual { actual: deleteBy (/=) 1 nil, expected: nil }

  log "difference (\\\\) should work with empty collections"
  assertEqual { actual: l [1, 2, 3, 4, 3, 2] `difference` nil, expected: l [1, 2, 3, 4, 3, 2] }
  assertEqual { actual: nil `difference` l [1, 2], expected: nil }
  assertEqual { actual: nil `difference` nil, expected: nil }

  log "drop should work with empty collections"
  assertEqual { actual: drop 1 nil, expected: nil }
  assertEqual { actual: drop (-1) nil, expected: nil }

  log "dropEnd should work with empty collections"
  assertEqual { actual: dropEnd 1 nil, expected: nil }
  assertEqual { actual: dropEnd 1 nil, expected: nil }

  log "dropWhile should work with empty collections"
  assertEqual { actual: dropWhile (_ /= 1) nil, expected: nil }

  log "filter should work with empty collections"
  assertEqual { actual: filter odd nil, expected: nil }

  log "filterM should work with empty collections"
  assertEqual { actual: filterM (Just <<< odd) nil, expected: Just $ l [] }
  assertEqual { actual: filterM (const Nothing) nil, expected: Just $ l [] }

  log "group should work with empty collections"
  assertEqual { actual: group nil, expected: l [] }

  log "groupAll should work with empty collections"
  assertSkip [SkipBrokenLazyCanEmpty]
   \_ -> { actual: groupAll nil, expected: l [] }

  log "groupAllBy should work with empty collections"
  assertEqual { actual: groupAllBy (compare `on` (_ `div` 10)) nil, expected: l [] }

  log "groupBy should work with empty collections"
  assertEqual { actual: groupBy (eq `on` (_ `mod` 10)) nil, expected: l [] }

  log "mapMaybe should work with empty collections"
  assertEqual { actual: mapMaybe (\x -> if x /= 0 then Just x else Nothing) nil, expected: nil }

  log "partition should work with empty collections"
  assertEqual { actual: partition (_ > 2) nil, expected: { yes: nil, no: nil } }

  log "slice should work with empty collections"
  assertEqual { actual: slice 1 3 nil, expected: nil }
  assertEqual { actual: slice (-2) 3 nil, expected: nil }

  log "span should work with empty collections"
  assertEqual { actual: span (_ < 4) nil, expected: { init: nil, rest: nil } }

  log "stripPrefix should work with empty collections"
  assertEqual { actual: stripPrefix (pattern nil) $ l [4, 2, 5, 1] , expected: Just $ l [4, 2, 5, 1] }
  assertEqual { actual: stripPrefix (pattern (l [4, 2])) nil , expected: Nothing }
  assertEqual { actual: stripPrefix (pattern nil) nil , expected: Just nil }

  log "take should work with empty collections"
  assertEqual { actual: take 1 nil, expected: nil }
  assertEqual { actual: take 0 nil, expected: nil }
  assertEqual { actual: take (-1) nil, expected: nil }

  log "takeEnd should work with empty collections"
  assertSkip [SkipBrokenLazyCanEmpty]
    \_ -> { actual: takeEnd 1 nil, expected: nil }
  assertSkip [SkipBrokenLazyCanEmpty]
    \_ -> { actual: takeEnd 0 nil, expected: nil }
  assertSkip [SkipBrokenLazyCanEmpty]
    \_ -> { actual: takeEnd (-1) nil, expected: nil }

  log "takeWhile should work with empty collections"
  assertEqual { actual: takeWhile (_ /= 2) nil, expected: nil }


-- Todo - test can-empty versions of common lazy functions



testOnlyNonEmpty :: forall c canEmpty cPattern.
  Comonad c =>
  --, Foldable1 c => -- missing from LazyNonEmptyList
  --, Traversable1 c => -- missing from LazyNonEmptyList
  Eq (c Int) =>
  Eq (canEmpty Int) =>
  Show (c Int) =>
  Show (canEmpty Int) =>
  -- parameters:
  OnlyNonEmpty c canEmpty ->
  CommonDiffEmptiability c canEmpty canEmpty c cPattern ->
  Effect Unit
testOnlyNonEmpty
  { makeCollection
  , makeCanEmptyCollection

  -- Only available for NonEmpty collections
  , fromList
  , toList
  -- Todo, should there be a `toUnfoldable1` function for NonEmpty collections?

  -- Can't be deduplicated from CanEmpty collections due to use of Maybe
  , fromFoldable
  , head
  , init
  , last
  , tail
  , uncons
  }

  -- CommonDiffEmptiability
  -- Only testing a few of these functions here
  { cons'
  , snoc'
  } = do
  let
    l = makeCollection
    cel = makeCanEmptyCollection

    nil :: canEmpty Int
    nil = cel []

  printTestType "Only nonEmpty"

  -- ======= Typeclass tests ========

  -- Todo

  -- Comonad
  -- Foldable1
  -- Traversable1

  -- ======= Functions tests ========

  -- These functions are only available for NonEmpty collections

  log "fromList should convert from a List to a NonEmptyList"
  assertEqual { actual: fromList (cel [1, 2, 3]), expected: Just $ l [1, 2, 3] }
  assertEqual { actual: fromList nil, expected: Nothing }

  log "toList should convert from a NonEmptyList to a List"
  assertEqual { actual: toList (l [1, 2, 3]), expected: cel [1, 2, 3] }


  -- These are the remaining functions that can't be deduplicated due to use of Maybe

  log "fromFoldable should create a collection from another foldable collection"
  assertEqual { actual: fromFoldable [1, 2], expected: Just $ l [1, 2] }
  assertEqual { actual: fromFoldable ([] :: _ Int), expected: Nothing }
  assertEqual { actual: fromFoldable (Just 1), expected: Just $ l [1] }
  assertEqual { actual: fromFoldable (Nothing :: _ Int), expected: Nothing }


  log "head should return a the first value"
  assertEqual { actual: head (l [1, 2]), expected: 1 }

  log "init should return a canEmpty collection of all but the last value"
  assertEqual { actual: init (l [1, 2, 3]), expected: cel [1, 2] }

  log "last should return the last value"
  assertEqual { actual: last (l [1, 2]), expected: 2 }

  log "tail should return a canEmpty collection of all but the first value"
  assertEqual { actual: tail (l [1, 2, 3]), expected: cel [2, 3] }

  log "uncons should split a collection into a record containing the first and remaining values"
  assertEqual { actual: uncons (l [1]), expected: { head: 1, tail: nil } }
  assertEqual { actual: uncons (l [1, 2, 3]), expected: { head: 1, tail: cel [2, 3] } }


  -- ========== Common API with additional canEmpty tests ==========
  -- Note that cons' and snoc' must be tested here, rather than in
  -- canEmpty, because we want a canEmpty arg and nonEmpty returned collection.

  log "cons' should work with empty collections"
  assertEqual { actual: cons' 1 nil, expected: l [1] }

  log "snoc' should work with empty collections"
  assertEqual { actual: snoc' nil 3, expected: l [3] }




testOnlyLazy :: forall c.
  Lazy (c Int) => -- Todo - missing from LazyNonEmptyList
  Eq (c Int) =>
  Show (c Int) =>
  OnlyLazy c ->
  Common c ->
  Effect Unit
testOnlyLazy
  { makeCollection
  , takeSimple

  -- Same names, but different APIs (without Maybe)
  , insertAt
  , modifyAt
  , updateAt

  -- These are only available for Lazy collections
  , iterate
  , repeat
  , cycle
  , foldrLazy
  , scanlLazy

  -- Specialized from Unfoldable1's replicate1 / replicate1A
  , replicate1
  , replicate1M
  }

  -- Reusing some Common functions for these Lazy tests.
  -- Note, must use `com` named pattern to work around this bug:
  -- https://github.com/purescript/purescript/issues/3938
  com@{
    cons
  , length
  , singleton
  } = do
  let
    l = makeCollection

  printTestType "Only Lazy"


  log "insertAt should add an item at the specified index"
  assertEqual { actual: insertAt 0 1 $ l [2, 3], expected: l [1, 2, 3] }
  assertEqual { actual: insertAt 1 1 $ l [2, 3], expected: l [2, 1, 3] }
  assertEqual { actual: insertAt 2 1 $ l [2, 3], expected: l [2, 3, 1] }

  log "insertAt should return the original collection if the index is out of range"
  assertEqual { actual: insertAt 7 8 $ l [1,2,3], expected: l [1,2,3] }
  assertEqual { actual: insertAt (-1) 8 $ l [1,2,3], expected: l [1,2,3] }


  log "modifyAt should update an item at the specified index"
  assertEqual { actual: modifyAt 0 (_ + 1) $ l [1, 2, 3], expected: l [2, 2, 3] }
  assertEqual { actual: modifyAt 1 (_ + 1) $ l [1, 2, 3], expected: l [1, 3, 3] }

  log "modifyAt should return the original collection if the index is out of range"
  assertEqual { actual: modifyAt 7 (_ + 1) $ l [1,2,3], expected: l [1,2,3] }
  assertEqual { actual: modifyAt (-1) (_ + 1) $ l [1,2,3], expected: l [1,2,3] }


  log "updateAt should replace an item at the specified index"
  assertEqual { actual: updateAt 0 9 $ l [1, 2, 3], expected: l [9, 2, 3] }
  assertEqual { actual: updateAt 1 9 $ l [1, 2, 3], expected: l [1, 9, 3] }

  log "updateAt should return the original collection if the index is out of range"
  assertEqual { actual: updateAt 5 9 $ l [1, 2, 3], expected: l [1, 2, 3] }
  assertEqual { actual: updateAt (-1) 9 $ l [1, 2, 3], expected: l [1, 2, 3] }

  let
    t5 = takeSimple 5

  log "repeate should create an infinite collection of a repeated element"
  assertEqual { actual: t5 $ repeat 2, expected: l [2, 2, 2, 2, 2] }

  log "cycle should create an infinite collection by repeating another collection"
  assertEqual { actual: t5 $ cycle (l [1, 2, 3]), expected: l [1, 2, 3, 1, 2] }

  log "iterate should create an infinite collection by iterating a function"
  assertEqual { actual: t5 $ iterate (mul 2) 3, expected: l [3, 6, 12, 24, 48] }


  log "foldrLazy should be correct"
  assertEqual
    { actual: foldrLazy (\a b -> (a * 2) `cons` b) (singleton (-1)) $ l [1, 2, 3]
    , expected: l [2, 4, 6, -1]
    }

  log "foldrLazy should work ok on infinite lists"
  assertEqual
    { actual: takeSimple 3 $ foldrLazy (\a b -> (a * 2) `cons` b) (singleton (-1)) $ iterate (add 1) 1
    , expected: l [2, 4, 6]
    }


  log "scanlLazy should be correct"
  assertEqual
    { actual: scanlLazy add 5 $ l [1, 2, 3, 4]
    , expected: l [6, 8, 11, 15]
    }

  log "scanlLazy should work ok on infinite lists"
  assertEqual
    { actual: takeSimple 4 $ scanlLazy add 5 $ iterate (add 1) 1
    , expected: l [6, 8, 11, 15]
    }


  log "replicate1 should be correct"
  assertEqual { actual: replicate1 3 1, expected: l [1, 1, 1] }
  assertEqual { actual: replicate1 1 1, expected: l [1] }
  assertEqual { actual: replicate1 0 1, expected: l [1] }
  assertEqual { actual: replicate1 (-1) 1, expected: l [1] }

  log "replicate1 should be stack-safe"
  assertEqual { actual: length (replicate1 100000 1), expected: 100000 }

  log "replicate1 should be lazy"
  assertEqual { actual: takeSimple 3 $ replicate1 100000000 1, expected: l [1, 1, 1] }


  log "replicate1M should be correct"
  assertEqual { actual: replicate1M 3 $ Just 1, expected: Just $ l [1, 1, 1] }
  assertEqual { actual: replicate1M 1 $ Just 1, expected: Just $ l [1] }
  assertEqual { actual: replicate1M 0 $ Just 1, expected: Just $ l [1] }
  assertEqual { actual: replicate1M (-1) $ Just 1, expected: Just $ l [1] }

  assertEqual { actual: replicate1M 3 (Nothing :: _ Int), expected: Nothing }
  assertEqual { actual: replicate1M 1 (Nothing :: _ Int), expected: Nothing }
  assertEqual { actual: replicate1M 0 (Nothing :: _ Int), expected: Nothing }
  assertEqual { actual: replicate1M (-1) (Nothing :: _ Int), expected: Nothing }

  log "replicate1M should be stack-safe"
  -- Must use com. here because of this typechecker bug:
  -- https://github.com/purescript/purescript/issues/3938#issuecomment-880390437
  assertEqual { actual: map com.length (replicate1M 100000 (Just 1)), expected: Just 100000 }
  assertEqual { actual: map com.length (replicate1M 100000 (Nothing :: _ Int)), expected: Nothing }

  log "replicate1M should be lazy"
  assertEqual { actual: map (takeSimple 3) $ replicate1M 100000000 $ Just 1, expected: Just $ l [1, 1, 1] }
  assertEqual { actual: map (takeSimple 3) $ replicate1M 100000000 $ (Nothing :: _ Int), expected: Nothing }


  -- Additional common tests for lazy collections

  -- todo, reorder

  log "nub should not consume more of the input list than necessary"
  assertEqual
    { actual: takeSimple 3 $ com.nub (cycle (l [1, 2, 3]))
    , expected: l [1, 2, 3]
    }


  -- Todo - nubEq, etc


  let
    nonZeroAdd 0 _ = Nothing
    nonZeroAdd _ 0 = Nothing
    nonZeroAdd x y = Just $ x + y

  log "zipWithA should work with infinite lists"
  assertEqual { actual: com.zipWithA nonZeroAdd (repeat 1) $ l [1, 2, 3], expected: Just $ l [11, 12, 13] }
  assertEqual { actual: com.zipWithA nonZeroAdd (l [1, 2, 3]) $ repeat 1, expected: Just $ l [11, 12, 13] }
  assertEqual { actual: com.zipWithA nonZeroAdd (repeat 1) $ cycle (l [1, 2, 0]), expected: Nothing }

  log "foldM should work ok on infinite lists"
  assertEqual { actual: com.foldM nonZeroAdd 1 $ cycle (l [1, 2, 0]), expected: Nothing }

  log "range should be lazy"
  assertEqual { actual: takeSimple 3 $ com.range 0 100000000, expected: l [0, 1, 2] }

testOnlyStrict :: forall c.
  Eq (c Int) =>
  Show (c Int) =>
  OnlyStrict c -> Effect Unit
testOnlyStrict
  { makeCollection

  -- Same names, but different APIs (with Maybe)
  , insertAt
  , modifyAt
  , updateAt

  } = do

  let
    l = makeCollection

  printTestType "Only Strict"

  log "insertAt should add an item at the specified index"
  assertEqual { actual: insertAt 0 1 $ l [2, 3], expected: Just $ l [1, 2, 3] }
  assertEqual { actual: insertAt 1 1 $ l [2, 3], expected: Just $ l [2, 1, 3] }
  assertEqual { actual: insertAt 2 1 $ l [2, 3], expected: Just $ l [2, 3, 1] }

  log "insertAt should return Nothing if the index is out of range"
  assertEqual { actual: insertAt 7 8 $ l [1,2,3], expected: Nothing }
  assertEqual { actual: insertAt (-1) 8 $ l [1,2,3], expected: Nothing }

  log "modifyAt should update an item at the specified index"
  assertEqual { actual: modifyAt 0 (_ + 1) $ l [1, 2, 3], expected: Just $ l [2, 2, 3] }
  assertEqual { actual: modifyAt 1 (_ + 1) $ l [1, 2, 3], expected: Just $ l [1, 3, 3] }

  log "modifyAt should return Nothing if the index is out of range"
  assertEqual { actual: modifyAt 7 (_ + 1) $ l [1,2,3], expected: Nothing }
  assertEqual { actual: modifyAt (-1) (_ + 1) $ l [1,2,3], expected: Nothing }

  log "updateAt should replace an item at the specified index"
  assertEqual { actual: updateAt 0 9 $ l [1, 2, 3], expected: Just $ l [9, 2, 3] }
  assertEqual { actual: updateAt 1 9 $ l [1, 2, 3], expected: Just $ l [1, 9, 3] }

  log "updateAt should return Nothing if the index is out of range"
  assertEqual { actual: updateAt 5 9 $ l [1, 2, 3], expected: Nothing }
  assertEqual { actual: updateAt (-1) 9 $ l [1, 2, 3], expected: Nothing }



-- Functions that cannot be tested generically.

-- helper func
removeZerosAndDouble :: Int -> Maybe Int
removeZerosAndDouble 0 = Nothing
removeZerosAndDouble n = Just $ 2 * n

testOnlyStrictCanEmpty :: OnlyStrictCanEmpty L.List -> Effect Unit
testOnlyStrictCanEmpty
  { alterAt
  , deleteAt
  } = do

  let
    l :: forall f a. Foldable f => f a -> L.List a
    l = L.fromFoldable

  printTestType "Only Strict canEmpty"

  -- Common function names, but different signatures

  log "alterAt should remove an item at the specified index"
  assertEqual { actual: alterAt 0 removeZerosAndDouble $ l [1, 2, 3], expected: Just $ l [2, 2, 3] }
  assertEqual { actual: alterAt 1 removeZerosAndDouble $ l [1, 0, 3], expected: Just $ l [1, 3] }

  log "alterAt should return Nothing if index is out of bounds"
  assertEqual { actual: alterAt 5 removeZerosAndDouble $ l [1, 2, 3], expected: Nothing }
  assertEqual { actual: alterAt (-1) removeZerosAndDouble $ l [1, 2, 3], expected: Nothing }


  log "deleteAt should remove an item at the specified index"
  assertEqual { actual: deleteAt 0 $ l [1, 2, 3], expected: Just $ l [2, 3] }
  assertEqual { actual: deleteAt 1 $ l [1, 2, 3], expected: Just $ l [1, 3] }

  log "deleteAt should return Nothing if index is out of bounds"
  assertEqual { actual: deleteAt 5 $ l [1, 2, 3], expected: Nothing }
  assertEqual { actual: deleteAt (-1) $ l [1, 2, 3], expected: Nothing }


testOnlyStrictNonEmpty :: OnlyStrictNonEmpty NEL.NonEmptyList L.List -> Effect Unit
testOnlyStrictNonEmpty
  { alterAt
  , deleteAt
  } = do

  let
    l :: forall f a. Foldable f => f a -> NEL.NonEmptyList a
    l = unsafePartial fromJust <<< NEL.fromFoldable

    cel :: forall f a. Foldable f => f a -> L.List a
    cel = L.fromFoldable

  printTestType "Only Strict NonEmpty"

  -- Common function names, but different signatures

  log "alterAt should remove an item at the specified index"
  assertEqual { actual: alterAt 0 removeZerosAndDouble $ l [1, 2, 3], expected: Just $ cel [2, 2, 3] }
  assertEqual { actual: alterAt 1 removeZerosAndDouble $ l [1, 0, 3], expected: Just $ cel [1, 3] }

  log "alterAt should return Nothing if index is out of bounds"
  assertEqual { actual: alterAt 5 removeZerosAndDouble $ l [1, 2, 3], expected: Nothing }
  assertEqual { actual: alterAt (-1) removeZerosAndDouble $ l [1, 2, 3], expected: Nothing }


  log "deleteAt should remove an item at the specified index"
  assertSkipAlways \_ -> { actual: deleteAt 0 $ l [1, 2, 3], expected: Just $ cel [2, 3] }
  assertSkipAlways \_ -> { actual: deleteAt 1 $ l [1, 2, 3], expected: Just $ cel [1, 3] }

  log "deleteAt should return Nothing if index is out of bounds"
  assertSkipAlways \_ -> { actual: deleteAt 5 $ l [1, 2, 3], expected: Nothing }
  assertSkipAlways \_ -> { actual: deleteAt (-1) $ l [1, 2, 3], expected: Nothing }


testOnlyLazyCanEmpty :: OnlyLazyCanEmpty LL.List -> Effect Unit
testOnlyLazyCanEmpty
  -- Common function names, but different signatures
  { alterAt
  , deleteAt
  -- Unique functions
  , replicate
  , replicateM
  } = do

  let
    l :: forall f a. Foldable f => f a -> LL.List a
    l = LL.fromFoldable

    nil = l []

  printTestType "Only Lazy canEmpty"

  -- Common function names, but different signatures

  log "alterAt should remove an item at the specified index"
  assertEqual { actual: alterAt 0 removeZerosAndDouble $ l [1, 2, 3], expected: l [2, 2, 3] }
  assertEqual { actual: alterAt 1 removeZerosAndDouble $ l [1, 0, 3], expected: l [1, 3] }

  log "alterAt should return the original collection if index is out of bounds"
  assertEqual { actual: alterAt 5 removeZerosAndDouble $ l [1, 2, 3], expected: l [1, 2, 3] }
  assertEqual { actual: alterAt (-1) removeZerosAndDouble $ l [1, 2, 3], expected: l [1, 2, 3] }


  log "deleteAt should remove an item at the specified index"
  assertEqual { actual: deleteAt 0 $ l [1, 2, 3], expected: l [2, 3] }
  assertEqual { actual: deleteAt 1 $ l [1, 2, 3], expected: l [1, 3] }

  log "deleteAt should return the original collection if index is out of bounds"
  assertEqual { actual: deleteAt 5 $ l [1, 2, 3], expected: l [1, 2, 3] }
  assertEqual { actual: deleteAt (-1) $ l [1, 2, 3], expected: l [1, 2, 3] }

  -- Unique functions

  log "replicate should be correct"
  assertEqual { actual: replicate 3 1, expected: l [1, 1, 1] }
  assertEqual { actual: replicate 1 1, expected: l [1] }
  assertEqual { actual: replicate 0 1, expected: nil }
  assertEqual { actual: replicate (-1) 1, expected: nil }

  log "replicate should be stack-safe"
  assertEqual { actual: LL.length (replicate 100000 1), expected: 100000 }

  log "replicate should be lazy"
  assertEqual { actual: LL.take 3 $ replicate 100000000 1, expected: l [1, 1, 1] }


  log "replicate should be correct"
  assertEqual { actual: replicateM 3 $ Just 1, expected: Just $ l [1, 1, 1] }
  assertEqual { actual: replicateM 1 $ Just 1, expected: Just $ l [1] }
  assertEqual { actual: replicateM 0 $ Just 1, expected: Just nil }
  assertEqual { actual: replicateM (-1) $ Just 1, expected: Just nil }

  assertEqual { actual: replicateM 3 (Nothing :: _ Int), expected: Nothing }
  assertEqual { actual: replicateM 1 (Nothing :: _ Int), expected: Nothing }
  assertEqual { actual: replicateM 0 (Nothing :: _ Int), expected: Nothing }
  assertEqual { actual: replicateM (-1) (Nothing :: _ Int), expected: Nothing }

  log "replicateM should be stack-safe"
  assertEqual { actual: map LL.length (replicateM 100000 (Just 1)), expected: Just 100000 }
  assertEqual { actual: map LL.length (replicateM 100000 (Nothing :: _ Int)), expected: Nothing }

  log "replicateM should be lazy"
  assertEqual { actual: map (LL.take 3) $ replicateM 100000000 $ Just 1, expected: Just $ l [1, 1, 1] }
  assertEqual { actual: map (LL.take 3) $ replicateM 100000000 $ (Nothing :: _ Int), expected: Nothing }

{-
  -- This currently only works for lazy-can-empty.
  -- It also requires access to some additional common functions.
  -- Might not be worth keeping.

  log "can find the first 10 primes using lazy collections"
  let eratos :: c Int -> c Int
      eratos xs = defer \_ ->
        case com.uncons xs of
          Nothing -> nil
          Just { head: p, tail: ys } -> p `cons` eratos (filter (\x -> x `mod` p /= 0) ys)

      primes = eratos $ iterate (add 1) 2

  assertEqual { actual: takeSimple 10 primes, expected: l [2, 3, 5, 7, 11, 13, 17, 19, 23, 29] }
-}



testOnlyLazyNonEmpty :: OnlyLazyNonEmpty LNEL.NonEmptyList LL.List -> Effect Unit
testOnlyLazyNonEmpty
  { alterAt
  , deleteAt
  } = do

  let
    l :: forall f a. Foldable f => f a -> LNEL.NonEmptyList a
    l = unsafePartial fromJust <<< LNEL.fromFoldable

    cel :: forall f a. Foldable f => f a -> LL.List a
    cel = LL.fromFoldable

  printTestType "Only Lazy NonEmpty"

  -- Common function names, but different signatures

  log "alterAt should remove an item at the specified index"
  assertEqual { actual: alterAt 0 removeZerosAndDouble $ l [1, 2, 3], expected: cel [2, 2, 3] }
  assertEqual { actual: alterAt 1 removeZerosAndDouble $ l [1, 0, 3], expected: cel [1, 3] }

  log "alterAt should return the original collection if index is out of bounds"
  assertEqual { actual: alterAt 5 removeZerosAndDouble $ l [1, 2, 3], expected: cel [1, 2, 3] }
  assertEqual { actual: alterAt (-1) removeZerosAndDouble $ l [1, 2, 3], expected: cel [1, 2, 3] }


  log "deleteAt should remove an item at the specified index"
  assertEqual { actual: deleteAt 0 $ l [1, 2, 3], expected: cel [2, 3] }
  assertEqual { actual: deleteAt 1 $ l [1, 2, 3], expected: cel [1, 3] }

  log "deleteAt should return the original collection if index is out of bounds"
  assertEqual { actual: deleteAt 5 $ l [1, 2, 3], expected: cel [1, 2, 3] }
  assertEqual { actual: deleteAt (-1) $ l [1, 2, 3], expected: cel [1, 2, 3] }
