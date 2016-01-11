## Module Data.List.ZipList

This module defines the type of _zip lists_, i.e. linked lists
with a zippy `Applicative` instance.

#### `ZipList`

``` purescript
newtype ZipList a
  = ZipList (List a)
```

`ZipList` is a newtype around `List` which provides a zippy
`Applicative` instance.

##### Instances
``` purescript
(Show a) => Show (ZipList a)
(Eq a) => Eq (ZipList a)
(Ord a) => Ord (ZipList a)
Semigroup (ZipList a)
Monoid (ZipList a)
Foldable ZipList
Traversable ZipList
Functor ZipList
Apply ZipList
Applicative ZipList
Alt ZipList
Plus ZipList
Alternative ZipList
```

#### `runZipList`

``` purescript
runZipList :: forall a. ZipList a -> List a
```

Unpack a `ZipList` to obtain the underlying list.


