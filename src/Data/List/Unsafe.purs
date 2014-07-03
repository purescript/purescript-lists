module Data.List.Unsafe where

import Data.List (List(..))

head :: forall a. List a -> a
head (Cons x _) = x

tail :: forall a. List a -> List a
tail (Cons _ xs) = xs
