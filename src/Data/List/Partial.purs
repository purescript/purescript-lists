-- | Partial helper functions for working with strict linked lists.
module Data.List.Partial where

import Data.List (List(..))

-- | Get the first element of a non-empty list.
-- |
-- | Running time: `O(1)`.
head :: forall a. Partial => List a -> a
head (Cons x _) = x

-- | Get all but the first element of a non-empty list.
-- |
-- | Running time: `O(1)`
tail :: forall a. Partial => List a -> List a
tail (Cons _ xs) = xs

-- | Get the last element of a non-empty list.
-- |
-- | Running time: `O(n)`
last :: forall a. Partial => List a -> a
last list = case list of
  Cons x Nil -> x
  Cons _ xs -> last xs

-- | Get all but the last element of a non-empty list.
-- |
-- | Running time: `O(n)`
init :: forall a. Partial => List a -> List a
init list = case list of
  Cons _ Nil -> Nil
  Cons x xs -> Cons x (init xs)
