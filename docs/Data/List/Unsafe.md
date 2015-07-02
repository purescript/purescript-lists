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


