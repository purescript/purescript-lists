module Data.List.Types where

import Prelude
import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)
import Data.Foldable (class Foldable, foldr, foldl, intercalate)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as NE
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)

data List a = Nil | Cons a (List a)

infixr 6 Cons as :

derive instance genericList :: Generic a => Generic (List a)

instance showList :: Show a => Show (List a) where
  show Nil = "Nil"
  show xs = "(" <> intercalate " : " (show <$> xs) <> " : Nil)"

instance eqList :: Eq a => Eq (List a) where
  eq xs ys = go xs ys true
    where
      go _ _ false = false
      go Nil Nil acc = acc
      go (x : xs') (y : ys') acc = go xs' ys' $ acc && (y == x)
      go _ _ _ = false

instance ordList :: Ord a => Ord (List a) where
  compare xs ys = go xs ys
    where
    go Nil Nil = EQ
    go Nil _ = LT
    go _ Nil = GT
    go (x : xs') (y : ys') =
      case compare x y of
        EQ -> go xs' ys'
        other -> other

instance semigroupList :: Semigroup (List a) where
  append xs ys = foldr (:) ys xs

instance monoidList :: Monoid (List a) where
  mempty = Nil

instance functorList :: Functor List where
  map f = foldr (\x acc -> f x : acc) Nil

instance foldableList :: Foldable List where
  foldr f b as = foldl (flip f) b (rev Nil as)
    where
    rev acc Nil = acc
    rev acc (x : xs) = rev (x : acc) xs
  foldl f = go
    where
    go b Nil = b
    go b (a : as) = go (f b a) as
  foldMap f = foldl (\acc -> append acc <<< f) mempty

instance unfoldableList :: Unfoldable List where
  unfoldr f b = go b Nil
    where
      go source memo = case f source of
        Nothing -> foldl (flip (:)) Nil memo
        Just (Tuple one rest) -> go rest (one : memo)

instance traversableList :: Traversable List where
  traverse f = map (foldl (flip (:)) Nil) <<< foldl (\acc -> lift2 (flip (:)) acc <<< f) (pure Nil)
  sequence = traverse id

instance applyList :: Apply List where
  apply Nil _ = Nil
  apply (f : fs) xs = (f <$> xs) <> (fs <*> xs)

instance applicativeList :: Applicative List where
  pure a = a : Nil

instance bindList :: Bind List where
  bind Nil _ = Nil
  bind (x : xs) f = f x <> bind xs f

instance monadList :: Monad List

instance altList :: Alt List where
  alt = append

instance plusList :: Plus List where
  empty = Nil

instance alternativeList :: Alternative List

instance monadZeroList :: MonadZero List

instance monadPlusList :: MonadPlus List

instance extendList :: Extend List where
  extend f Nil = Nil
  extend f l@(a : as) =
    f l : (foldr go { val: Nil, acc: Nil } as).val
    where
    go a' { val, acc } =
      let acc' = a' : acc
      in { val: f acc' : val, acc: acc' }

newtype NonEmptyList a = NonEmptyList (NonEmpty List a)

toList :: NonEmptyList ~> List
toList (NonEmptyList (x :| xs)) = x : xs

derive instance newtypeNonEmptyList :: Newtype (NonEmptyList a) _

derive newtype instance eqNonEmptyList :: Eq a => Eq (NonEmptyList a)
derive newtype instance ordNonEmptyList :: Ord a => Ord (NonEmptyList a)
derive newtype instance genericEmptyList :: Generic a => Generic (NonEmptyList a)

instance showNonEmptyList :: Show a => Show (NonEmptyList a) where
  show (NonEmptyList nel) = "(NonEmptyList " <> show nel <> ")"

derive newtype instance functorNonEmptyList :: Functor NonEmptyList

instance applyNonEmptyList :: Apply NonEmptyList where
  apply (NonEmptyList (f :| fs)) (NonEmptyList (a :| as)) =
    NonEmptyList (f a :| (fs <*> a : Nil) <> ((f : fs) <*> as))

instance applicativeNonEmptyList :: Applicative NonEmptyList where
  pure = NonEmptyList <<< NE.singleton

instance bindNonEmptyList :: Bind NonEmptyList where
  bind (NonEmptyList (a :| as)) f =
    case f a of
      NonEmptyList (b :| bs) ->
        NonEmptyList (b :| bs <> bind as (toList <<< f))

instance monadNonEmptyList :: Monad NonEmptyList

instance altNonEmptyList :: Alt NonEmptyList where
  alt = append

instance extendNonEmptyList :: Extend NonEmptyList where
  extend f w@(NonEmptyList (_ :| as)) =
    NonEmptyList (f w :| (foldr go { val: Nil, acc: Nil } as).val)
    where
    go a { val, acc } = { val: f (NonEmptyList (a :| acc)) : val, acc: a : acc }

instance comonadNonEmptyList :: Comonad NonEmptyList where
  extract (NonEmptyList (a :| _)) = a

instance semigroupNonEmptyList :: Semigroup (NonEmptyList a) where
  append (NonEmptyList (a :| as)) as' =
    NonEmptyList (a :| as <> toList as')

derive newtype instance foldableNonEmptyList :: Foldable NonEmptyList

derive newtype instance traversableNonEmptyList :: Traversable NonEmptyList
