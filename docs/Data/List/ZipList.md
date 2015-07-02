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
instance showZipList :: (Show a) => Show (ZipList a)
instance eqZipList :: (Eq a) => Eq (ZipList a)
instance ordZipList :: (Ord a) => Ord (ZipList a)
instance semigroupZipList :: Semigroup (ZipList a)
instance monoidZipList :: Monoid (ZipList a)
instance foldableZipList :: Foldable ZipList
instance traversableZipList :: Traversable ZipList
instance functorZipList :: Functor ZipList
instance applyZipList :: Apply ZipList
instance applicativeZipList :: Applicative ZipList
instance altZipList :: Alt ZipList
instance plusZipList :: Plus ZipList
instance alternativeZipList :: Alternative ZipList
```

#### `runZipList`

``` purescript
runZipList :: forall a. ZipList a -> List a
```

Unpack a `ZipList` to obtain the underlying list.


