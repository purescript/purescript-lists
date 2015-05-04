# Module Documentation

## Module Data.List


This module defines a type of _strict_ linked lists, and associated helper
functions and type class instances.

_Note_: Depending on your use-case, you may prefer to use
`Data.Sequence` instead, which might give better performance for certain
use cases. This module is an improvement over `Data.Array` when working with 
immutable lists of data in a purely-functional setting, but does not have 
good random-access performance.

#### `List`

``` purescript
data List a
  = Nil 
  | Cons a (List a)
```

A strict linked list.

A list is either empty (represented by the `Nil` constructor) or non-empty, in
which case it consists of a head element, and another list (represented by the 
`Cons` constructor).

#### `fromArray`

``` purescript
fromArray :: forall a. [a] -> List a
```

Construct a list from an immutable array.

Running time: `O(n)`

#### `toArray`

``` purescript
toArray :: forall a. List a -> [a]
```

Convert a list into an immutable array.

Running time: `O(n)`

#### `(:)`

``` purescript
(:) :: forall a. a -> List a -> List a
```

An infix alias for `Cons`; attaches an element to the front of 
a list.

Running time: `O(1)`

#### `singleton`

``` purescript
singleton :: forall a. a -> List a
```

Create a list with a single element.

Running time: `O(1)`

#### `index`

``` purescript
index :: forall a. List a -> Number -> Maybe a
```

Get the element at the specified index, or `Nothing` if the index is out-of-bounds.

Running time: `O(n)` where `n` is the required index.

#### `(!!)`

``` purescript
(!!) :: forall a. List a -> Number -> Maybe a
```

An infix synonym for `index`.

#### `drop`

``` purescript
drop :: forall a. Number -> List a -> List a
```

Drop the specified number of elements from the front of a list.

Running time: `O(n)` where `n` is the number of elements to drop.

#### `dropWhile`

``` purescript
dropWhile :: forall a. (a -> Boolean) -> List a -> List a
```

Drop those elements from the front of a list which match a predicate.

Running time (worst case): `O(n)`

#### `take`

``` purescript
take :: forall a. Number -> List a -> List a
```

Take the specified number of elements from the front of a list.

Running time: `O(n)` where `n` is the number of elements to take.

#### `takeWhile`

``` purescript
takeWhile :: forall a. (a -> Boolean) -> List a -> List a
```

Take those elements from the front of a list which match a predicate.

Running time (worst case): `O(n)`

#### `length`

``` purescript
length :: forall a. List a -> Number
```

Get the length of a list

Running time: `O(n)`

#### `filter`

``` purescript
filter :: forall a. (a -> Boolean) -> List a -> List a
```

Filter a list, keeping the elements which satisfy a predicate function.

Running time: `O(n)`

#### `mapMaybe`

``` purescript
mapMaybe :: forall a b. (a -> Maybe b) -> List a -> List b
```

Apply a function to each element in a list, keeping only the results which
contain a value.

Running time: `O(n)`

#### `catMaybes`

``` purescript
catMaybes :: forall a. List (Maybe a) -> List a
```

Filter a list of optional values, keeping only the elements which contain
a value.

#### `head`

``` purescript
head :: forall a. List a -> Maybe a
```

Get the first element in a list, or `Nothing` if the list is empty.

Running time: `O(1)`.

#### `tail`

``` purescript
tail :: forall a. List a -> Maybe (List a)
```

Get all but the first element of a list, or `Nothing` if the list is empty.

Running time: `O(1)`

#### `uncons`

``` purescript
uncons :: forall a. List a -> Maybe (Tuple a (List a))
```

Break a list into its first element, and the remaining elements,
or `Nothing` if the list is empty.

Running time: `O(1)`

#### `last`

``` purescript
last :: forall a. List a -> Maybe a
```

Get the last element in a list, or `Nothing` if the list is empty.

Running time: `O(1)`.

#### `init`

``` purescript
init :: forall a. List a -> Maybe (List a)
```

Get all but the last element of a list, or `Nothing` if the list is empty.

Running time: `O(1)`

#### `zipWith`

``` purescript
zipWith :: forall a b c. (a -> b -> c) -> List a -> List b -> List c
```

Apply a function to pairs of elements at the same positions in two lists,
collecting the results in a new list.

If one list is longer, elements will be discarded from the longer list.

For example

```purescript
zipWith (*) (1 : 2 : 3 : Nil) (4 : 5 : 6 : 7 Nil) == 4 : 10 : 18 : Nil
```

Running time: `O(min(m, n))`

#### `zip`

``` purescript
zip :: forall a b. List a -> List b -> List (Tuple a b)
```

Collect pairs of elements at the same positions in two lists.

Running time: `O(min(m, n))`

#### `concat`

``` purescript
concat :: forall a. List (List a) -> List a
```

Flatten a list of lists.

Running time: `O(n)`, where `n` is the total number of elements.

#### `concatMap`

``` purescript
concatMap :: forall a b. (a -> List b) -> List a -> List b
```

Apply a function to each element in a list, and flatten the results
into a single, new list.

Running time: `O(n)`, where `n` is the total number of elements.

#### `null`

``` purescript
null :: forall a. List a -> Boolean
```

Test whether a list is empty.

Running time: `O(1)`

#### `span`

``` purescript
span :: forall a. (a -> Boolean) -> List a -> Tuple (List a) (List a)
```

Split a list into two parts:

1. the longest initial segment for which all elements satisfy the specified predicate
2. the remaining elements

For example,

```purescript
span (\n -> n % 2 == 1) (1 : 3 : 2 : 4 : 5 : Nil) == Tuple (1 : 3 : Nil) (2 : 4 : 5 : Nil)
```

Running time: `O(n)`

#### `group`

``` purescript
group :: forall a. (Eq a) => List a -> List (List a)
```

Group equal, consecutive elements of a list into lists.

For example,

```purescript
group (1 : 1 : 2 : 2 : 1 : Nil) == (1 : 1 : Nil) : (2 : 2 : Nil) : (1 : Nil) : Nil
```

Running time: `O(n)`

#### `groupBy`

``` purescript
groupBy :: forall a. (a -> a -> Boolean) -> List a -> List (List a)
```

Group equal, consecutive elements of a list into lists, using the specified
equivalence relation to determine equality.

Running time: `O(n)`

#### `(\\)`

``` purescript
(\\) :: forall a. (Eq a) => List a -> List a -> List a
```

Delete the first occurrence of each element in the second list from the first list.

Running time: `O(n^2)`

#### `insert`

``` purescript
insert :: forall a. (Ord a) => a -> List a -> List a
```

Insert an element into a sorted list.

Running time: `O(n)`

#### `insertBy`

``` purescript
insertBy :: forall a. (a -> a -> Ordering) -> a -> List a -> List a
```

Insert an element into a sorted list, using the specified function to determine the ordering
of elements.

Running time: `O(n)`

#### `insertAt`

``` purescript
insertAt :: forall a. Number -> a -> List a -> Maybe (List a)
```

Insert an element into a list at the specified index, returning a new
list or `Nothing` if the index is out-of-bounds.

Running time: `O(n)`

#### `delete`

``` purescript
delete :: forall a. (Eq a) => a -> List a -> List a
```

Delete the first occurrence of an element from a list.

Running time: `O(n)`

#### `deleteBy`

``` purescript
deleteBy :: forall a. (a -> a -> Boolean) -> a -> List a -> List a
```

Delete the first occurrence of an element from a list, using the specified 
function to determine equality of elements.

Running time: `O(n)`

#### `deleteAt`

``` purescript
deleteAt :: forall a. Number -> List a -> Maybe (List a)
```

Delete an element from a list at the specified index, returning a new
list or `Nothing` if the index is out-of-bounds.

Running time: `O(n)`

#### `updateAt`

``` purescript
updateAt :: forall a. Number -> a -> List a -> Maybe (List a)
```

Update the element at the specified index, returning a new
list or `Nothing` if the index is out-of-bounds.

Running time: `O(n)`

#### `modifyAt`

``` purescript
modifyAt :: forall a. Number -> (a -> a) -> List a -> Maybe (List a)
```

Update the element at the specified index by applying a function to
the current value, returning a new list or `Nothing` if the index is 
out-of-bounds.

Running time: `O(n)`

#### `alterAt`

``` purescript
alterAt :: forall a. Number -> (a -> Maybe a) -> List a -> Maybe (List a)
```

Update or delete the element at the specified index by applying a 
function to the current value, returning a new list or `Nothing` if the 
index is out-of-bounds.

Running time: `O(n)`

#### `reverse`

``` purescript
reverse :: forall a. List a -> List a
```

Reverse a list.

Running time: `O(n)`

#### `nub`

``` purescript
nub :: forall a. (Eq a) => List a -> List a
```

Remove duplicate elements from a list.

Running time: `O(n^2)`

#### `nubBy`

``` purescript
nubBy :: forall a. (a -> a -> Boolean) -> List a -> List a
```

Remove duplicate elements from a list, using the specified 
function to determine equality of elements.

Running time: `O(n^2)`

#### `intersect`

``` purescript
intersect :: forall a. (Eq a) => List a -> List a -> List a
```

Calculate the intersection of two lists.

Running time: `O(n^2)`

#### `intersectBy`

``` purescript
intersectBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a
```

Calculate the intersection of two lists, using the specified 
function to determine equality of elements.

Running time: `O(n^2)`

#### `union`

``` purescript
union :: forall a. (Eq a) => List a -> List a -> List a
```

Calculate the union of two lists.

Running time: `O(n^2)`

#### `unionBy`

``` purescript
unionBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a
```

Calculate the union of two lists, using the specified 
function to determine equality of elements.

Running time: `O(n^2)`

#### `showList`

``` purescript
instance showList :: (Show a) => Show (List a)
```


#### `eqList`

``` purescript
instance eqList :: (Eq a) => Eq (List a)
```


#### `ordList`

``` purescript
instance ordList :: (Ord a) => Ord (List a)
```


#### `semigroupList`

``` purescript
instance semigroupList :: Semigroup (List a)
```


#### `monoidList`

``` purescript
instance monoidList :: Monoid (List a)
```


#### `functorList`

``` purescript
instance functorList :: Functor List
```


#### `foldableList`

``` purescript
instance foldableList :: Foldable List
```


#### `unfoldableList`

``` purescript
instance unfoldableList :: Unfoldable List
```


#### `traversableList`

``` purescript
instance traversableList :: Traversable List
```


#### `applyList`

``` purescript
instance applyList :: Apply List
```


#### `applicativeList`

``` purescript
instance applicativeList :: Applicative List
```


#### `bindList`

``` purescript
instance bindList :: Bind List
```


#### `monadList`

``` purescript
instance monadList :: Monad List
```


#### `altList`

``` purescript
instance altList :: Alt List
```


#### `plusList`

``` purescript
instance plusList :: Plus List
```


#### `alternativeList`

``` purescript
instance alternativeList :: Alternative List
```


#### `monadPlusList`

``` purescript
instance monadPlusList :: MonadPlus List
```



## Module Data.List.Unsafe


Unsafe helper functions for working with strict linked lists.

_Note_: these functions should be used with care, and may result in unspecified
behavior, including runtime exceptions.

#### `head`

``` purescript
head :: forall a. List a -> a
```

Get the first element of a non-empty list.

Running time: `O(1)`.

#### `tail`

``` purescript
tail :: forall a. List a -> List a
```

Get all but the first element of a non-empty list.

Running time: `O(1)`

#### `last`

``` purescript
last :: forall a. List a -> a
```

Get the last element of a non-empty list.

Running time: `O(n)`

#### `init`

``` purescript
init :: forall a. List a -> List a
```

Get all but the last element of a non-empty list.

Running time: `O(n)`



