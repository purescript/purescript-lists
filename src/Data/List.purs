-- | This module defines a type of _strict_ linked lists, and associated helper
-- | functions and type class instances.
-- | 
-- | _Note_: Depending on your use-case, you may prefer to use
-- | `Data.Sequence` instead, which might give better performance for certain
-- | use cases. This module is an improvement over `Data.Array` when working with 
-- | immutable lists of data in a purely-functional setting, but does not have 
-- | good random-access performance.

module Data.List 
  ( List(..)
  , (:)
  , singleton
  , fromArray
  , toArray
  , index
  , (!!)
  , drop
  , dropWhile
  , take
  , takeWhile
  , length
  , filter
  , mapMaybe
  , catMaybes
  , head
  , tail
  , last
  , init
  , zipWith
  , zip
  , concat
  , concatMap
  , null
  , span
  , group
  , groupBy
  , (\\)
  , insert
  , insertBy
  , insertAt
  , delete
  , deleteBy
  , deleteAt
  , updateAt
  , modifyAt
  , alterAt
  , reverse
  , nub
  , nubBy
  , intersect
  , intersectBy
  , uncons
  , union
  , unionBy
  ) where

import Prelude hiding ((:))

import Data.Maybe
import Data.Tuple (Tuple(..))
import Data.Monoid
import Data.Foldable
import Data.Unfoldable
import Data.Traversable

import Control.Alt
import Control.Plus
import Control.Alternative
import Control.MonadPlus

-- | A strict linked list.
-- |
-- | A list is either empty (represented by the `Nil` constructor) or non-empty, in
-- | which case it consists of a head element, and another list (represented by the 
-- | `Cons` constructor).
data List a = Nil | Cons a (List a)

-- | Construct a list from an immutable array.
-- |
-- | Running time: `O(n)`
fromArray :: forall a. [a] -> List a
fromArray = foldr Cons Nil

-- | Convert a list into an immutable array.
-- |
-- | Running time: `O(n)`
toArray :: forall a. List a -> [a]
toArray = unfoldr step
  where
  step :: forall a. List a -> Maybe (Tuple a (List a))
  step Nil = Nothing
  step (Cons x xs) = Just (Tuple x xs)

infixr 6 :

-- | An infix alias for `Cons`; attaches an element to the front of 
-- | a list.
-- |
-- | Running time: `O(1)`
(:) :: forall a. a -> List a -> List a
(:) = Cons

-- | Create a list with a single element.
-- |
-- | Running time: `O(1)`
singleton :: forall a. a -> List a
singleton a = Cons a Nil

-- | Get the element at the specified index, or `Nothing` if the index is out-of-bounds.
-- |
-- | Running time: `O(n)` where `n` is the required index.
index :: forall a. List a -> Number -> Maybe a
index Nil _ = Nothing
index (Cons a _) 0 = Just a
index (Cons _ as) i = index as (i - 1)

infix 4 !!

-- | An infix synonym for `index`.
(!!) :: forall a. List a -> Number -> Maybe a
(!!) = index

-- | Drop the specified number of elements from the front of a list.
-- |
-- | Running time: `O(n)` where `n` is the number of elements to drop.
drop :: forall a. Number -> List a -> List a
drop 0 xs = xs
drop _ Nil = Nil
drop n (Cons x xs) = drop (n - 1) xs

-- | Drop those elements from the front of a list which match a predicate.
-- |
-- | Running time (worst case): `O(n)`
dropWhile :: forall a. (a -> Boolean) -> List a -> List a
dropWhile p = go
  where
  go (Cons x xs) | p x = go xs
  go xs = xs

-- | Take the specified number of elements from the front of a list.
-- |
-- | Running time: `O(n)` where `n` is the number of elements to take.
take :: forall a. Number -> List a -> List a
take = go Nil
  where 
  go acc 0 _ = reverse acc
  go acc _ Nil = reverse acc
  go acc n (Cons x xs) = go (Cons x acc) (n - 1) xs

-- | Take those elements from the front of a list which match a predicate.
-- |
-- | Running time (worst case): `O(n)`
takeWhile :: forall a. (a -> Boolean) -> List a -> List a
takeWhile p = go Nil
  where
  go acc (Cons x xs) | p x = go (Cons x acc) xs
  go acc _ = reverse acc

-- | Get the length of a list
-- |
-- | Running time: `O(n)`
length :: forall a. List a -> Number
length Nil = 0
length (Cons _ xs) = 1 + length xs

-- | Filter a list, keeping the elements which satisfy a predicate function.
-- |
-- | Running time: `O(n)`
filter :: forall a. (a -> Boolean) -> List a -> List a
filter p = go Nil
  where
  go acc Nil = reverse acc
  go acc (Cons x xs) 
    | p x = go (Cons x acc) xs
    | otherwise = go acc xs

-- | Apply a function to each element in a list, keeping only the results which
-- | contain a value.
-- |
-- | Running time: `O(n)`
mapMaybe :: forall a b. (a -> Maybe b) -> List a -> List b
mapMaybe f = go Nil
  where
  go acc Nil = reverse acc
  go acc (Cons x xs) =
    case f x of
      Nothing -> go acc xs
      Just y -> go (Cons y acc) xs

-- | Filter a list of optional values, keeping only the elements which contain
-- | a value.
catMaybes :: forall a. List (Maybe a) -> List a
catMaybes = mapMaybe id

-- | Get the first element in a list, or `Nothing` if the list is empty.
-- |
-- | Running time: `O(1)`.
head :: forall a. List a -> Maybe a
head Nil = Nothing
head (Cons x _) = Just x

-- | Get all but the first element of a list, or `Nothing` if the list is empty.
-- |
-- | Running time: `O(1)`
tail :: forall a. List a -> Maybe (List a)
tail Nil = Nothing
tail (Cons _ xs) = Just xs

-- | Break a list into its first element, and the remaining elements,
-- | or `Nothing` if the list is empty.
-- |
-- | Running time: `O(1)`
uncons :: forall a. List a -> Maybe (Tuple a (List a))
uncons Nil = Nothing
uncons (Cons x xs) = Just $ Tuple x xs

-- | Get the last element in a list, or `Nothing` if the list is empty.
-- |
-- | Running time: `O(1)`.
last :: forall a. List a -> Maybe a
last (Cons x Nil) = Just x
last (Cons _ xs)  = last xs
last _            = Nothing

-- | Get all but the last element of a list, or `Nothing` if the list is empty.
-- |
-- | Running time: `O(1)`
init :: forall a. List a -> Maybe (List a)
init (Cons x Nil) = Just Nil
init (Cons x xs)  = Cons x <$> init xs
init _            = Nothing

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
zipWith _ Nil _ = Nil
zipWith _ _ Nil = Nil
zipWith f (Cons a as) (Cons b bs) = Cons (f a b) (zipWith f as bs)

-- | Collect pairs of elements at the same positions in two lists.
-- |
-- | Running time: `O(min(m, n))`
zip :: forall a b. List a -> List b -> List (Tuple a b)
zip = zipWith Tuple

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
concatMap _ Nil = Nil
concatMap f (Cons x xs) = f x <> concatMap f xs

-- | Test whether a list is empty.
-- |
-- | Running time: `O(1)`
null :: forall a. List a -> Boolean
null Nil = true
null _ = false

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
span :: forall a. (a -> Boolean) -> List a -> Tuple (List a) (List a)
span p xs@(Cons x xs') 
  | p x = case span p xs' of
            Tuple ys zs -> Tuple (Cons x ys) zs
  | otherwise = Tuple Nil xs

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
groupBy _ Nil = Nil
groupBy eq (Cons x xs) = 
  case span (eq x) xs of
    Tuple ys zs -> Cons (Cons x ys) (groupBy eq zs)

infix 5 \\

-- | Delete the first occurrence of each element in the second list from the first list.
-- |
-- | Running time: `O(n^2)`
(\\) :: forall a. (Eq a) => List a -> List a -> List a
(\\) = foldl (flip delete)

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
insertBy _ x Nil = Cons x Nil
insertBy cmp x ys@(Cons y ys') =
  case cmp x y of
    GT -> Cons y (insertBy cmp x ys')
    _  -> Cons x ys

-- | Insert an element into a list at the specified index, returning a new
-- | list or `Nothing` if the index is out-of-bounds.
-- |
-- | Running time: `O(n)`
insertAt :: forall a. Number -> a -> List a -> Maybe (List a)
insertAt 0 x xs = Just (Cons x xs)
insertAt n x (Cons y ys) = Cons y <$> insertAt (n - 1) x ys
insertAt _ _ _  = Nothing

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
deleteBy _ _ Nil = Nil
deleteBy (==) x (Cons y ys) | x == y = ys 
deleteBy (==) x (Cons y ys) = Cons y (deleteBy (==) x ys)

-- | Delete an element from a list at the specified index, returning a new
-- | list or `Nothing` if the index is out-of-bounds.
-- |
-- | Running time: `O(n)`
deleteAt :: forall a. Number -> List a -> Maybe (List a)
deleteAt 0 (Cons y ys) = Just ys
deleteAt n (Cons y ys) = Cons y <$> deleteAt (n - 1) ys
deleteAt _ _  = Nothing

-- | Update the element at the specified index, returning a new
-- | list or `Nothing` if the index is out-of-bounds.
-- |
-- | Running time: `O(n)`
updateAt :: forall a. Number -> a -> List a -> Maybe (List a)
updateAt 0 x (Cons _ xs) = Just (Cons x xs)
updateAt n x (Cons x1 xs) = Cons x1 <$> updateAt (n - 1) x xs
updateAt _ _ _ = Nothing

-- | Update the element at the specified index by applying a function to
-- | the current value, returning a new list or `Nothing` if the index is 
-- | out-of-bounds.
-- |
-- | Running time: `O(n)`
modifyAt :: forall a. Number -> (a -> a) -> List a -> Maybe (List a)
modifyAt n f = alterAt n (Just <<< f)

-- | Update or delete the element at the specified index by applying a 
-- | function to the current value, returning a new list or `Nothing` if the 
-- | index is out-of-bounds.
-- |
-- | Running time: `O(n)`
alterAt :: forall a. Number -> (a -> Maybe a) -> List a -> Maybe (List a)
alterAt 0 f (Cons y ys) = Just $
  case f y of
    Nothing -> ys
    Just y' -> Cons y' ys
alterAt n f (Cons y ys) = Cons y <$> alterAt (n - 1) f ys
alterAt _ _ _  = Nothing

-- | Reverse a list.
-- |
-- | Running time: `O(n)`
reverse :: forall a. List a -> List a
reverse = go Nil
  where
  go acc Nil = acc
  go acc (Cons x xs) = go (Cons x acc) xs

-- | Remove duplicate elements from a list.
-- |
-- | Running time: `O(n^2)`
nub :: forall a. (Eq a) => List a -> List a
nub = nubBy (==)

-- | Remove duplicate elements from a list, using the specified 
-- | function to determine equality of elements.
-- |
-- | Running time: `O(n^2)`
nubBy :: forall a. (a -> a -> Boolean) -> List a -> List a
nubBy _     Nil = Nil
nubBy (==) (Cons x xs) = Cons x (nubBy (==) (filter (\y -> not (x == y)) xs))

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
intersectBy _  Nil _   = Nil
intersectBy _  _   Nil = Nil
intersectBy eq xs  ys  = filter (\x -> any (eq x) ys) xs

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

instance showList :: (Show a) => Show (List a) where
  show Nil = "Nil"
  show (Cons x xs) = "Cons (" ++ show x ++ ") (" ++ show xs ++ ")"

instance eqList :: (Eq a) => Eq (List a) where
  (==) Nil Nil = true
  (==) (Cons x xs) (Cons y ys) = x == y && xs == ys
  (==) _ _ = false
  (/=) xs ys = not (xs == ys)

instance ordList :: (Ord a) => Ord (List a) where
  compare Nil Nil = EQ
  compare Nil _   = LT
  compare _   Nil = GT
  compare (Cons x xs) (Cons y ys) = 
    case compare x y of
      EQ -> compare xs ys
      other -> other

instance semigroupList :: Semigroup (List a) where
  (<>) Nil ys = ys
  (<>) (Cons x xs) ys = Cons x (xs <> ys)

instance monoidList :: Monoid (List a) where
  mempty = Nil

instance functorList :: Functor List where
  (<$>) _ Nil = Nil
  (<$>) f (Cons x xs) = Cons (f x) (f <$> xs)

instance foldableList :: Foldable List where
  -- foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldr _ b Nil = b
  foldr o b (Cons a as) = a `o` foldr o b as

  -- foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldl _ b Nil = b
  foldl o b (Cons a as) = foldl o (b `o` a) as

  -- foldMap :: forall a m. (Monoid m) => (a -> m) -> f a -> m 
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs 

instance unfoldableList :: Unfoldable List where
  -- unfoldr :: forall a b. (b -> Maybe (Tuple a b)) -> b -> List a
  unfoldr f b = go (f b)
    where
    go Nothing = Nil
    go (Just (Tuple a b)) = Cons a (go (f b))

instance traversableList :: Traversable List where
  -- traverse :: forall a b m. (Applicative m) => (a -> m b) -> t a -> m (t b)
  traverse _ Nil = pure Nil
  traverse f (Cons a as) = Cons <$> f a <*> traverse f as

  -- sequence :: forall a m. (Applicative m) => t (m a) -> m (t a)   
  sequence Nil = pure Nil
  sequence (Cons a as) = Cons <$> a <*> sequence as

instance applyList :: Apply List where
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) xs = (f <$> xs) <> (fs <*> xs)

instance applicativeList :: Applicative List where
  pure a = Cons a Nil

instance bindList :: Bind List where
  (>>=) = flip concatMap

instance monadList :: Monad List

instance altList :: Alt List where
  (<|>) = (<>)

instance plusList :: Plus List where
  empty = Nil

instance alternativeList :: Alternative List 

instance monadPlusList :: MonadPlus List