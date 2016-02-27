-- | This module defines a type of _lazy_ linked lists, and associated helper
-- | functions and type class instances.
-- |
-- | _Note_: Depending on your use-case, you may prefer to use
-- | `Data.Sequence` instead, which might give better performance for certain
-- | use cases. This module is an improvement over `Data.Array` when working with
-- | immutable lists of data in a purely-functional setting, but does not have
-- | good random-access performance.

module Data.List.Lazy
  ( List(..)
  , runList
  , toUnfoldable
  , fromFoldable
  , Step(..)
  , step
  , nil

  , singleton
  , (..), range
  -- , replicate
  -- , replicateM
  -- , some
  -- , many
  , repeat
  , iterate
  , cycle

  , null
  , length

  , (:), cons
  -- , snoc
  , insert
  , insertBy

  , head
  , last
  , tail
  , init
  , uncons

  , (!!), index
  -- , elemIndex
  -- , elemLastIndex
  -- , findIndex
  -- , findLastIndex
  , insertAt
  , deleteAt
  , updateAt
  , modifyAt
  , alterAt

  , reverse
  , concat
  , concatMap
  , filter
  -- , filterM
  , mapMaybe
  , catMaybes

  -- , sort
  -- , sortBy

  -- , slice
  , take
  , takeWhile
  , drop
  , dropWhile
  , span
  , group
  -- , group'
  , groupBy

  , nub
  , nubBy
  , union
  , unionBy
  , delete
  , deleteBy
  , (\\)
  , intersect
  , intersectBy

  , zipWith
  -- , zipWithA
  , zip
  -- , unzip

  , transpose

  -- , foldM
  , toList
  , fromList
  ) where

import Prelude

import Control.Alt (Alt)
import Control.Alternative (Alternative)
import Control.MonadPlus (MonadPlus)
import Control.Plus (Plus)
import qualified Control.Lazy as Z

import Data.Foldable (Foldable, foldMap, foldl, foldr, any)
import Data.Lazy (Lazy(), defer, force)
import Data.Maybe (Maybe(..), isNothing)
import Data.Monoid (Monoid, mempty)
import Data.Traversable (Traversable, traverse, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (Unfoldable, unfoldr)


-- | A lazy linked list.
newtype List a = List (Lazy (Step a))

-- | Unwrap a lazy linked list
runList :: forall a. List a -> Lazy (Step a)
runList (List l) = l

-- | Convert a list into any unfoldable structure.
-- |
-- | Running time: `O(n)`
toUnfoldable :: forall f a. (Unfoldable f) => List a -> f a
toUnfoldable = unfoldr (\xs -> (\rec -> Tuple rec.head rec.tail) <$> uncons xs)

-- | Construct a list from a foldable structure.
-- |
-- | Running time: `O(n)`
fromFoldable :: forall f a. (Foldable f) => f a -> List a
fromFoldable = foldr cons nil

-- | A list is either empty (represented by the `Nil` constructor) or non-empty, in
-- | which case it consists of a head element, and another list (represented by the
-- | `Cons` constructor).
data Step a = Nil | Cons a (List a)

fromStep :: forall a. Step a -> List a
fromStep = List <<< pure

-- | Unwrap a lazy linked list
step :: forall a. List a -> Step a
step = force <<< runList

-- | The empty list.
-- |
-- | Running time: `O(1)`
nil :: forall a. List a
nil = List $ defer \_ -> Nil

--------------------------------------------------------------------------------
-- List creation ---------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Create a list with a single element.
-- |
-- | Running time: `O(1)`
singleton :: forall a. a -> List a
singleton a = cons a nil

-- | An infix synonym for `range`.
(..) :: Int -> Int -> List Int
(..) = range

-- | Create a list containing a range of integers, including both endpoints.
range :: Int -> Int -> List Int
range start end | start == end = singleton start
                | otherwise = go end start (if start > end then 1 else -1) nil
  where
  go s e step' rest | s == e = (cons s rest)
                    | otherwise = go (s + step') e step' (cons s rest)

-- | Create a list by repeating an element
repeat :: forall a. a -> List a
repeat x = Z.fix \xs -> cons x xs

-- | Create a list by iterating a function
iterate :: forall a. (a -> a) -> a -> List a
iterate f x = Z.fix \xs -> cons x (f <$> xs)

-- | Create a list by repeating another list
cycle :: forall a. List a -> List a
cycle xs = Z.fix \ys -> xs <> ys

--------------------------------------------------------------------------------
-- List size -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Test whether a list is empty.
-- |
-- | Running time: `O(1)`
null :: forall a. List a -> Boolean
null = isNothing <<< uncons

-- | Get the length of a list
-- |
-- | Running time: `O(n)`
length :: forall a. List a -> Int
length xs = go (step xs)
  where
  go Nil = 0
  go (Cons _ xs) = 1 + go (step xs)

--------------------------------------------------------------------------------
-- Extending arrays ------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Attach an element to the front of a lazy list.
-- |
-- | Running time: `O(1)`
cons :: forall a. a -> List a -> List a
cons x xs = List $ defer \_ -> Cons x xs

infixr 6 :

-- | An infix alias for `cons`; attaches an element to the front of
-- | a list.
-- |
-- | Running time: `O(1)`
(:) :: forall a. a -> List a -> List a
(:) = cons

-- | Insert an element into a sorted list.
-- |
-- | Running time: `O(n)`
insert :: forall a. (Ord a) => a -> List a -> List a
insert = insertBy compare

-- | Insert an element into a sorted list, using the specified function to determine the ordering
-- | of elements.
-- |
-- | Running time: `O(n)`
insertBy :: forall a. (a -> a -> Ordering) -> a -> List a -> List a
insertBy cmp x xs = List (go <$> runList xs)
  where
  go Nil = Cons x nil
  go ys@(Cons y ys') =
    case cmp x y of
      GT -> Cons y (insertBy cmp x ys')
      _  -> Cons x (fromStep ys)

--------------------------------------------------------------------------------
-- Non-indexed reads -----------------------------------------------------------
--------------------------------------------------------------------------------

-- | Get the first element in a list, or `Nothing` if the list is empty.
-- |
-- | Running time: `O(1)`.
head :: forall a. List a -> Maybe a
head xs = _.head <$> uncons xs

-- | Get the last element in a list, or `Nothing` if the list is empty.
-- |
-- | Running time: `O(n)`.
last :: forall a. List a -> Maybe a
last xs = go (step xs)
  where
  go (Cons x xs) | null xs = Just x
                 | otherwise = go (step xs)
  go _            = Nothing

-- | Get all but the first element of a list, or `Nothing` if the list is empty.
-- |
-- | Running time: `O(1)`
tail :: forall a. List a -> Maybe (List a)
tail xs = _.tail <$> uncons xs

-- | Get all but the last element of a list, or `Nothing` if the list is empty.
-- |
-- | Running time: `O(n)`
init :: forall a. List a -> Maybe (List a)
init xs = go (step xs)
  where
  go :: Step a -> Maybe (List a)
  go (Cons x xs) | null xs = Just nil
                 | otherwise = cons x <$> go (step xs)
  go _            = Nothing

-- | Break a list into its first element, and the remaining elements,
-- | or `Nothing` if the list is empty.
-- |
-- | Running time: `O(1)`
uncons :: forall a. List a -> Maybe { head :: a, tail :: List a }
uncons xs = case step xs of
              Nil -> Nothing
              Cons x xs -> Just { head: x, tail: xs }

--------------------------------------------------------------------------------
-- Indexed operations ----------------------------------------------------------
--------------------------------------------------------------------------------

-- | Get the element at the specified index, or `Nothing` if the index is out-of-bounds.
-- |
-- | Running time: `O(n)` where `n` is the required index.
index :: forall a. List a -> Int -> Maybe a
index xs = go (step xs)
  where
  go Nil _ = Nothing
  go (Cons a _) 0 = Just a
  go (Cons _ as) i = go (step as) (i - 1)

infixl 8 !!

-- | An infix synonym for `index`.
(!!) :: forall a. List a -> Int -> Maybe a
(!!) = index

-- | Insert an element into a list at the specified index, returning a new
-- | list or `Nothing` if the index is out-of-bounds.
-- |
-- | This function differs from the strict equivalent in that out-of-bounds arguments
-- | result in the element being appended at the _end_ of the list.
-- |
-- | Running time: `O(n)`
insertAt :: forall a. Int -> a -> List a -> List a
insertAt 0 x xs = cons x xs
insertAt n x xs = List (go <$> runList xs)
  where
  go Nil = Cons x nil
  go (Cons y ys) = Cons y (insertAt (n - 1) x ys)

-- | Delete an element from a list at the specified index, returning a new
-- | list or `Nothing` if the index is out-of-bounds.
-- |
-- | This function differs from the strict equivalent in that out-of-bounds arguments
-- | result in the original list being returned unchanged.
-- |
-- | Running time: `O(n)`
deleteAt :: forall a. Int -> List a -> List a
deleteAt n xs = List (go n <$> runList xs)
  where
  go _ Nil = Nil
  go 0 (Cons y ys) = step ys
  go n (Cons y ys) = Cons y (deleteAt (n - 1) ys)

-- | Update the element at the specified index, returning a new
-- | list or `Nothing` if the index is out-of-bounds.
-- |
-- | This function differs from the strict equivalent in that out-of-bounds arguments
-- | result in the original list being returned unchanged.
-- |
-- | Running time: `O(n)`
updateAt :: forall a. Int -> a -> List a -> List a
updateAt n x xs = List (go n <$> runList xs)
  where
  go _ Nil = Nil
  go 0 (Cons _ ys) = Cons x ys
  go n (Cons y ys) = Cons y (updateAt (n - 1) x ys)

-- | Update the element at the specified index by applying a function to
-- | the current value, returning a new list or `Nothing` if the index is
-- | out-of-bounds.
-- |
-- | This function differs from the strict equivalent in that out-of-bounds arguments
-- | result in the original list being returned unchanged.
-- |
-- | Running time: `O(n)`
modifyAt :: forall a. Int -> (a -> a) -> List a -> List a
modifyAt n f = alterAt n (Just <<< f)

-- | Update or delete the element at the specified index by applying a
-- | function to the current value, returning a new list or `Nothing` if the
-- | index is out-of-bounds.
-- |
-- | This function differs from the strict equivalent in that out-of-bounds arguments
-- | result in the original list being returned unchanged.
-- |
-- | Running time: `O(n)`
alterAt :: forall a. Int -> (a -> Maybe a) -> List a -> List a
alterAt n f xs = List (go n <$> runList xs)
  where
  go _ Nil = Nil
  go 0 (Cons y ys) = case f y of
    Nothing -> step ys
    Just y' -> Cons y' ys
  go n (Cons y ys) = Cons y (alterAt (n - 1) f ys)

--------------------------------------------------------------------------------
-- Transformations -------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Reverse a list.
-- |
-- | Running time: `O(n)`
reverse :: forall a. List a -> List a
reverse xs = go nil (step xs)
  where
  go acc Nil = acc
  go acc (Cons x xs) = go (cons x acc) (step xs)

-- | Flatten a list of lists.
-- |
-- | Running time: `O(n)`, where `n` is the total number of elements.
concat :: forall a. List (List a) -> List a
concat = (>>= id)

-- | Apply a function to each element in a list, and flatten the results
-- | into a single, new list.
-- |
-- | Running time: `O(n)`, where `n` is the total number of elements.
concatMap :: forall a b. (a -> List b) -> List a -> List b
concatMap f xs = List (go <$> runList xs)
  where
  go Nil = Nil
  go (Cons x xs) = step (f x <> concatMap f xs)

-- | Filter a list, keeping the elements which satisfy a predicate function.
-- |
-- | Running time: `O(n)`
filter :: forall a. (a -> Boolean) -> List a -> List a
filter p xs = List (go <$> runList xs)
  where
  go Nil = Nil
  go (Cons x xs)
    | p x = Cons x (filter p xs)
    | otherwise = go (step xs)

-- | Apply a function to each element in a list, keeping only the results which
-- | contain a value.
-- |
-- | Running time: `O(n)`
mapMaybe :: forall a b. (a -> Maybe b) -> List a -> List b
mapMaybe f xs = List (go <$> runList xs)
  where
  go Nil = Nil
  go (Cons x xs) =
    case f x of
      Nothing -> go (step xs)
      Just y -> Cons y (mapMaybe f xs)

-- | Filter a list of optional values, keeping only the elements which contain
-- | a value.
catMaybes :: forall a. List (Maybe a) -> List a
catMaybes = mapMaybe id

--------------------------------------------------------------------------------
-- Sorting ---------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Sublists --------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Take the specified number of elements from the front of a list.
-- |
-- | Running time: `O(n)` where `n` is the number of elements to take.
take :: forall a. Int -> List a -> List a
take n xs = List (go n <$> runList xs)
  where
  go :: Int -> Step a -> Step a
  go 0 _ = Nil
  go _ Nil = Nil
  go n (Cons x xs) = Cons x (take (n - 1) xs)

-- | Take those elements from the front of a list which match a predicate.
-- |
-- | Running time (worst case): `O(n)`
takeWhile :: forall a. (a -> Boolean) -> List a -> List a
takeWhile p xs = List (go <$> runList xs)
  where
  go (Cons x xs) | p x = Cons x (takeWhile p xs)
  go _ = Nil

-- | Drop the specified number of elements from the front of a list.
-- |
-- | Running time: `O(n)` where `n` is the number of elements to drop.
drop :: forall a. Int -> List a -> List a
drop n xs = List (go n <$> runList xs)
  where
  go 0 xs = xs
  go _ Nil = Nil
  go n (Cons x xs) = go (n - 1) (step xs)

-- | Drop those elements from the front of a list which match a predicate.
-- |
-- | Running time (worst case): `O(n)`
dropWhile :: forall a. (a -> Boolean) -> List a -> List a
dropWhile p xs = go (step xs)
  where
  go (Cons x xs) | p x = go (step xs)
  go xs = fromStep xs

-- | Split a list into two parts:
-- |
-- | 1. the longest initial segment for which all elements satisfy the specified predicate
-- | 2. the remaining elements
-- |
-- | For example,
-- |
-- | ```purescript
-- | span (\n -> n % 2 == 1) (1 : 3 : 2 : 4 : 5 : Nil) == Tuple (1 : 3 : Nil) (2 : 4 : 5 : Nil)
-- | ```
-- |
-- | Running time: `O(n)`
span :: forall a. (a -> Boolean) -> List a -> { init :: List a, rest :: List a }
span p xs =
  case uncons xs of
    Just { head: x, tail: xs' } | p x ->
      case span p xs' of
        { init: ys, rest: zs } -> { init: cons x ys, rest: zs }
    _ -> { init: nil, rest: xs }

-- | Group equal, consecutive elements of a list into lists.
-- |
-- | For example,
-- |
-- | ```purescript
-- | group (1 : 1 : 2 : 2 : 1 : Nil) == (1 : 1 : Nil) : (2 : 2 : Nil) : (1 : Nil) : Nil
-- | ```
-- |
-- | Running time: `O(n)`
group :: forall a. (Eq a) => List a -> List (List a)
group = groupBy (==)

-- | Group equal, consecutive elements of a list into lists, using the specified
-- | equivalence relation to determine equality.
-- |
-- | Running time: `O(n)`
groupBy :: forall a. (a -> a -> Boolean) -> List a -> List (List a)
groupBy eq xs = List (go <$> runList xs)
  where
  go Nil = Nil
  go (Cons x xs) =
    case span (eq x) xs of
      { init: ys, rest: zs } -> Cons (cons x ys) (groupBy eq zs)

--------------------------------------------------------------------------------
-- Set-like operations ---------------------------------------------------------
--------------------------------------------------------------------------------

-- | Remove duplicate elements from a list.
-- |
-- | Running time: `O(n^2)`
nub :: forall a. (Eq a) => List a -> List a
nub = nubBy eq

-- | Remove duplicate elements from a list, using the specified
-- | function to determine equality of elements.
-- |
-- | Running time: `O(n^2)`
nubBy :: forall a. (a -> a -> Boolean) -> List a -> List a
nubBy eq xs = List (go <$> runList xs)
  where
  go Nil = Nil
  go (Cons x xs) = Cons x (nubBy eq (filter (\y -> not (eq x y)) xs))

-- | Calculate the union of two lists.
-- |
-- | Running time: `O(n^2)`
union :: forall a. (Eq a) => List a -> List a -> List a
union = unionBy (==)

-- | Calculate the union of two lists, using the specified
-- | function to determine equality of elements.
-- |
-- | Running time: `O(n^2)`
unionBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a
unionBy eq xs ys = xs <> foldl (flip (deleteBy eq)) (nubBy eq ys) xs

-- | Delete the first occurrence of an element from a list.
-- |
-- | Running time: `O(n)`
delete :: forall a. (Eq a) => a -> List a -> List a
delete = deleteBy (==)

-- | Delete the first occurrence of an element from a list, using the specified
-- | function to determine equality of elements.
-- |
-- | Running time: `O(n)`
deleteBy :: forall a. (a -> a -> Boolean) -> a -> List a -> List a
deleteBy eq x xs = List (go <$> runList xs)
  where
  go Nil = Nil
  go (Cons y ys) | eq x y = step ys
                 | otherwise = Cons y (deleteBy eq x ys)

infix 5 \\

-- | Delete the first occurrence of each element in the second list from the first list.
-- |
-- | Running time: `O(n^2)`
(\\) :: forall a. (Eq a) => List a -> List a -> List a
(\\) = foldl (flip delete)

-- | Calculate the intersection of two lists.
-- |
-- | Running time: `O(n^2)`
intersect :: forall a. (Eq a) => List a -> List a -> List a
intersect = intersectBy (==)

-- | Calculate the intersection of two lists, using the specified
-- | function to determine equality of elements.
-- |
-- | Running time: `O(n^2)`
intersectBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a
intersectBy eq xs ys = filter (\x -> any (eq x) ys) xs

--------------------------------------------------------------------------------
-- Zipping ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Apply a function to pairs of elements at the same positions in two lists,
-- | collecting the results in a new list.
-- |
-- | If one list is longer, elements will be discarded from the longer list.
-- |
-- | For example
-- |
-- | ```purescript
-- | zipWith (*) (1 : 2 : 3 : Nil) (4 : 5 : 6 : 7 Nil) == 4 : 10 : 18 : Nil
-- | ```
-- |
-- | Running time: `O(min(m, n))`
zipWith :: forall a b c. (a -> b -> c) -> List a -> List b -> List c
zipWith f xs ys = List (go <$> runList xs <*> runList ys)
  where
  go :: Step a -> Step b -> Step c
  go Nil _ = Nil
  go _ Nil = Nil
  go (Cons a as) (Cons b bs) = Cons (f a b) (zipWith f as bs)

-- | Collect pairs of elements at the same positions in two lists.
-- |
-- | Running time: `O(min(m, n))`
zip :: forall a b. List a -> List b -> List (Tuple a b)
zip = zipWith Tuple

--------------------------------------------------------------------------------
-- Transpose -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The 'transpose' function transposes the rows and columns of its argument.
-- | For example,
-- |
-- |     transpose ((1:2:3:nil) : (4:5:6:nil) : nil) ==
-- |       ((1:4:nil) : (2:5:nil) : (3:6:nil) : nil)
-- |
-- | If some of the rows are shorter than the following rows, their elements are skipped:
-- |
-- |     transpose ((10:11:nil) : (20:nil) : nil : (30:31:32:nil) : nil) ==
-- |       ((10:20:30:nil) : (11:31:nil) : (32:nil) : nil)
transpose :: forall a. List (List a) -> List (List a)
transpose xs =
  case uncons xs of
    Nothing ->
      xs
    Just { head: h, tail: xss } ->
      case uncons h of
        Nothing ->
          transpose xss
        Just { head: x, tail: xs } ->
          (x : mapMaybe head xss) : transpose (xs : mapMaybe tail xss)

--------------------------------------------------------------------------------
-- Deprecated functions --------------------------------------------------------
--------------------------------------------------------------------------------

-- | *Deprecated.* Use `fromFoldable` instead. `toList` will be removed in a
-- | later version.
toList :: forall f a. (Foldable f) => f a -> List a
toList = fromFoldable

-- | *Deprecated.* Use `toUnfoldable` instead. `fromList` will be removed in a
-- | later version.
fromList :: forall f a. (Unfoldable f) => List a -> f a
fromList = toUnfoldable

--------------------------------------------------------------------------------
-- Instances -------------------------------------------------------------------
--------------------------------------------------------------------------------

instance showList :: (Show a) => Show (List a) where
  show xs = "fromStrict (" ++ go (step xs) ++ ")"
    where
    go Nil = "Nil"
    go (Cons x xs) = "Cons (" ++ show x ++ ") (" ++ go (step xs) ++ ")"

instance eqList :: (Eq a) => Eq (List a) where
  eq xs ys = go (step xs) (step ys)
    where
    go Nil Nil = true
    go (Cons x xs) (Cons y ys)
      | x == y = go (step xs) (step ys)
    go _ _ = false

instance ordList :: (Ord a) => Ord (List a) where
  compare xs ys = go (step xs) (step ys)
    where
    go Nil Nil = EQ
    go Nil _   = LT
    go _   Nil = GT
    go (Cons x xs) (Cons y ys) =
      case compare x y of
        EQ -> go (step xs) (step ys)
        other -> other

instance lazyList :: Z.Lazy (List a) where
  defer f = List $ defer (step <<< f)

instance semigroupList :: Semigroup (List a) where
  append xs ys = List (go <$> runList xs)
    where
    go Nil = step ys
    go (Cons x xs) = Cons x (xs <> ys)

instance monoidList :: Monoid (List a) where
  mempty = nil

instance functorList :: Functor List where
  map f xs = List (go <$> runList xs)
    where
    go Nil = Nil
    go (Cons x xs) = Cons (f x) (f <$> xs)

instance foldableList :: Foldable List where
  -- foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldr o b xs = go (step xs)
    where
    go Nil = b
    go (Cons a as) = a `o` foldr o b as

  -- foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldl o b xs = go (step xs)
    where
    go Nil = b
    go (Cons a as) = foldl o (b `o` a) as

  -- foldMap :: forall a m. (Monoid m) => (a -> m) -> f a -> m
  foldMap f xs = go (step xs)
    where
    go Nil = mempty
    go (Cons x xs) = f x <> foldMap f xs

instance unfoldableList :: Unfoldable List where
  -- unfoldr :: forall a b. (b -> Maybe (Tuple a b)) -> b -> List a
  unfoldr f b = go (f b)
    where
    go Nothing = nil
    -- go (Just (Tuple a b)) = cons a (go (f b))
    go (Just (Tuple a b)) = a : Z.defer \_ -> go (f b)

instance traversableList :: Traversable List where
  -- traverse :: forall a b m. (Applicative m) => (a -> m b) -> t a -> m (t b)
  traverse f xs = go (step xs)
    where
    go Nil = pure nil
    go (Cons x xs) = cons <$> f x <*> traverse f xs

  -- sequence :: forall a m. (Applicative m) => t (m a) -> m (t a)
  sequence xs = go (step xs)
    where
    go Nil = pure nil
    go (Cons x xs) = cons <$> x <*> sequence xs

instance applyList :: Apply List where
  apply = ap

instance applicativeList :: Applicative List where
  pure = singleton

instance bindList :: Bind List where
  bind = flip concatMap

instance monadList :: Monad List

instance altList :: Alt List where
  alt = append

instance plusList :: Plus List where
  empty = nil

instance alternativeList :: Alternative List

instance monadPlusList :: MonadPlus List
