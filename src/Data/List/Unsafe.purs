-- | Unsafe helper functions for working with strict linked lists.
-- |
-- | _Note_: these functions should be used with care, and may result in unspecified
-- | behavior, including runtime exceptions.

module Data.List.Unsafe
  ( head
  , tail
  , last
  , init
  ) where

import Prelude
import Data.List (List(..))

-- | Get the first element of a non-empty list.
-- |
-- | Running time: `O(1)`.
head :: forall a. List a -> a
head (Cons x _) = x
head Nil = unsafeThrow "Data.List.Unsafe.head called on empty list"

-- | Get all but the first element of a non-empty list.
-- |
-- | Running time: `O(1)`
tail :: forall a. List a -> List a
tail (Cons _ xs) = xs
tail Nil = unsafeThrow "Data.List.Unsafe.tail called on empty list"

-- | Get the last element of a non-empty list.
-- |
-- | Running time: `O(n)`
last :: forall a. List a -> a
last (Cons x Nil) = x
last (Cons _ xs)  = last xs
last Nil = unsafeThrow "Data.List.Unsafe.last called on empty list"

-- | Get all but the last element of a non-empty list.
-- |
-- | Running time: `O(n)`
init :: forall a. List a -> List a
init (Cons x Nil) = Nil
init (Cons x xs)  = Cons x (init xs)
init Nil = unsafeThrow "Data.List.Unsafe.init called on empty list"

foreign import unsafeThrow :: forall a. String -> a
