module Data.List (
    List(..)
  , fromList
  , (!)
  ) where

import Data.Maybe
import Data.Monoid
import Data.Foldable
import Data.Traversable

data List a = Nil | Cons a (List a)

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
  (>>=) Nil _ = Nil
  (>>=) (Cons a as) f = f a <> (as >>= f)

instance monadList :: Monad List

instance alternativeList :: Alternative List where
  empty = Nil
  (<|>) = (<>)

fromList :: forall a. [a] -> List a
fromList = foldr Cons Nil

infix 4 !

(!) :: forall a. List a -> Number -> Maybe a
(!) Nil _ = Nothing
(!) (Cons a _) 0 = Just a
(!) (Cons _ as) i = as ! i - 1

