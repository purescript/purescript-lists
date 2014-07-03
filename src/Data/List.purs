module Data.List (
    List(..)
  , fromArray
  , toArray
  , (!)
  , drop
  , take
  , length
  , filter
  , mapMaybe
  , catMaybes
  , head
  , tail
  , last
  , init) where

import Data.Maybe
import Data.Tuple
import Data.Monoid
import Data.Foldable
import Data.Unfoldable
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
  (>>=) Nil _ = Nil
  (>>=) (Cons a as) f = f a <> (as >>= f)

instance monadList :: Monad List

instance alternativeList :: Alternative List where
  empty = Nil
  (<|>) = (<>)

fromArray :: forall a. [a] -> List a
fromArray = foldr Cons Nil

toArray :: forall a. List a -> [a]
toArray = unfoldr step
  where
  step :: forall a. List a -> Maybe (Tuple a (List a))
  step Nil = Nothing
  step (Cons x xs) = Just (Tuple x xs)

infix 4 !

(!) :: forall a. List a -> Number -> Maybe a
(!) Nil _ = Nothing
(!) (Cons a _) 0 = Just a
(!) (Cons _ as) i = as ! i - 1

drop :: forall a. Number -> List a -> List a
drop 0 xs = xs
drop _ Nil = Nil
drop n (Cons x xs) = drop (n - 1) xs

take :: forall a. Number -> List a -> List a
take 0 _ = Nil
take _ Nil = Nil
take n (Cons x xs) = Cons x (take (n - 1) xs)

length :: forall a. List a -> Number
length Nil = 0
length (Cons _ xs) = 1 + length xs

filter :: forall a. (a -> Boolean) -> List a -> List a
filter _ Nil = Nil
filter p (Cons x xs) | p x = Cons x (filter p xs)
filter p (Cons _ xs) = filter p xs

mapMaybe :: forall a b. (a -> Maybe b) -> List a -> List b
mapMaybe _ Nil = Nil
mapMaybe f (Cons x xs) =
  case f x of
    Nothing -> mapMaybe f xs
    Just y -> Cons y (mapMaybe f xs)

catMaybes :: forall a. List (Maybe a) -> List a
catMaybes = mapMaybe id

head :: forall a. List a -> Maybe a
head Nil = Nothing
head (Cons x _) = Just x

tail :: forall a. List a -> Maybe (List a)
tail Nil = Nothing
tail (Cons _ xs) = Just xs

last :: forall a. List a -> Maybe a
last (Cons x Nil) = Just x
last (Cons _ xs)  = last xs
last _            = Nothing

init :: forall a. List a -> Maybe (List a)
init (Cons x Nil) = Just Nil
init (Cons x xs)  = Cons x <$> init xs
init _            = Nothing
