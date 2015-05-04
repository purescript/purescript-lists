# Module Documentation

## Module Data.List

#### `List`

``` purescript
data List a
  = Nil 
  | Cons a (List a)
```


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


#### `fromArray`

``` purescript
fromArray :: forall a. [a] -> List a
```


#### `toArray`

``` purescript
toArray :: forall a. List a -> [a]
```


#### `(:)`

``` purescript
(:) :: forall a. a -> List a -> List a
```

An infix alias for `Cons`.

#### `singleton`

``` purescript
singleton :: forall a. a -> List a
```


#### `(!!)`

``` purescript
(!!) :: forall a. List a -> Number -> Maybe a
```


#### `drop`

``` purescript
drop :: forall a. Number -> List a -> List a
```


#### `dropWhile`

``` purescript
dropWhile :: forall a. (a -> Boolean) -> List a -> List a
```


#### `take`

``` purescript
take :: forall a. Number -> List a -> List a
```


#### `takeWhile`

``` purescript
takeWhile :: forall a. (a -> Boolean) -> List a -> List a
```


#### `length`

``` purescript
length :: forall a. List a -> Number
```


#### `filter`

``` purescript
filter :: forall a. (a -> Boolean) -> List a -> List a
```


#### `mapMaybe`

``` purescript
mapMaybe :: forall a b. (a -> Maybe b) -> List a -> List b
```


#### `catMaybes`

``` purescript
catMaybes :: forall a. List (Maybe a) -> List a
```


#### `head`

``` purescript
head :: forall a. List a -> Maybe a
```


#### `tail`

``` purescript
tail :: forall a. List a -> Maybe (List a)
```


#### `uncons`

``` purescript
uncons :: forall a. List a -> Maybe (Tuple a (List a))
```


#### `last`

``` purescript
last :: forall a. List a -> Maybe a
```


#### `init`

``` purescript
init :: forall a. List a -> Maybe (List a)
```


#### `zipWith`

``` purescript
zipWith :: forall a b c. (a -> b -> c) -> List a -> List b -> List c
```


#### `concat`

``` purescript
concat :: forall a. List (List a) -> List a
```


#### `concatMap`

``` purescript
concatMap :: forall a b. (a -> List b) -> List a -> List b
```


#### `null`

``` purescript
null :: forall a. List a -> Boolean
```


#### `span`

``` purescript
span :: forall a. (a -> Boolean) -> List a -> Tuple (List a) (List a)
```


#### `group`

``` purescript
group :: forall a. (Eq a) => List a -> List (List a)
```


#### `groupBy`

``` purescript
groupBy :: forall a. (a -> a -> Boolean) -> List a -> List (List a)
```


#### `(\\)`

``` purescript
(\\) :: forall a. (Eq a) => List a -> List a -> List a
```


#### `insert`

``` purescript
insert :: forall a. (Ord a) => a -> List a -> List a
```


#### `insertBy`

``` purescript
insertBy :: forall a. (a -> a -> Ordering) -> a -> List a -> List a
```


#### `insertAt`

``` purescript
insertAt :: forall a. Number -> a -> List a -> Maybe (List a)
```


#### `delete`

``` purescript
delete :: forall a. (Eq a) => a -> List a -> List a
```


#### `deleteBy`

``` purescript
deleteBy :: forall a. (a -> a -> Boolean) -> a -> List a -> List a
```


#### `deleteAt`

``` purescript
deleteAt :: forall a. Number -> List a -> Maybe (List a)
```


#### `updateAt`

``` purescript
updateAt :: forall a. Number -> a -> List a -> Maybe (List a)
```


#### `modifyAt`

``` purescript
modifyAt :: forall a. Number -> (a -> a) -> List a -> Maybe (List a)
```


#### `alterAt`

``` purescript
alterAt :: forall a. Number -> (a -> Maybe a) -> List a -> Maybe (List a)
```


#### `reverse`

``` purescript
reverse :: forall a. List a -> List a
```


#### `nub`

``` purescript
nub :: forall a. (Eq a) => List a -> List a
```


#### `nubBy`

``` purescript
nubBy :: forall a. (a -> a -> Boolean) -> List a -> List a
```


#### `intersect`

``` purescript
intersect :: forall a. (Eq a) => List a -> List a -> List a
```


#### `intersectBy`

``` purescript
intersectBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a
```


#### `union`

``` purescript
union :: forall a. (Eq a) => List a -> List a -> List a
```


#### `unionBy`

``` purescript
unionBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a
```



## Module Data.List.Unsafe

#### `head`

``` purescript
head :: forall a. List a -> a
```


#### `tail`

``` purescript
tail :: forall a. List a -> List a
```


#### `last`

``` purescript
last :: forall a. List a -> a
```


#### `init`

``` purescript
init :: forall a. List a -> List a
```




