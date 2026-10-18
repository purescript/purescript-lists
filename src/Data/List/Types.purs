module Data.List.Types
  ( List(..)
  , (:)
  , NonEmptyList(..)
  , toList
  , nelCons
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Plus)
import Data.Eq (class Eq1, eq1)
import Data.Foldable (class Foldable, foldl, foldr, intercalate)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as NE
import Data.Ord (class Ord1, compare1)
import Data.Semigroup.Foldable (class Foldable1)
import Data.Semigroup.Traversable (class Traversable1, traverse1)
import Data.Traversable (class Traversable, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..), snd)
import Data.Unfoldable (class Unfoldable)
import Data.Unfoldable1 (class Unfoldable1)

data List a = Nil | Cons a (List a)

infixr 6 Cons as :

instance showList :: Show a => Show (List a) where
  show xs = case xs of
    Nil -> "Nil"
    _ -> "(" <> intercalate " : " (show <$> xs) <> " : Nil)"

instance eqList :: Eq a => Eq (List a) where
  eq = eq1

instance eq1List :: Eq1 List where
  eq1 xs ys = go xs ys true
    where
    go left right acc = case left, right, acc of
      _, _, false -> false
      Nil, Nil, _ -> acc
      x : xs', y : ys', _ -> go xs' ys' $ acc && (y == x)
      _, _, _ -> false

instance ordList :: Ord a => Ord (List a) where
  compare = compare1

instance ord1List :: Ord1 List where
  compare1 xs ys = go xs ys
    where
    go left right = case left, right of
      Nil, Nil -> EQ
      Nil, _ -> LT
      _, Nil -> GT
      x : xs', y : ys' ->
        case compare x y of
          EQ -> go xs' ys'
          other -> other

instance semigroupList :: Semigroup (List a) where
  append xs ys = foldr (:) ys xs

instance monoidList :: Monoid (List a) where
  mempty = Nil

instance functorList :: Functor List where
  map = listMap

-- chunked list Functor inspired by OCaml
-- https://discuss.ocaml.org/t/a-new-list-map-that-is-both-stack-safe-and-fast/865
-- chunk sizes determined through experimentation
listMap :: forall a b. (a -> b) -> List a -> List b
listMap f = chunkedRevMap Nil
  where
  chunkedRevMap :: List (List a) -> List a -> List b
  chunkedRevMap chunksAcc list = case list of
    chunk@(_ : _ : _ : xs) ->
      chunkedRevMap (chunk : chunksAcc) xs
    xs -> do
      let
        unrolledMap :: List a -> List b
        unrolledMap rest = case rest of
          x1 : x2 : Nil -> f x1 : f x2 : Nil
          x1 : Nil -> f x1 : Nil
          _ -> Nil

        reverseUnrolledMap :: List (List a) -> List b -> List b
        reverseUnrolledMap chunks acc = case chunks of
          (x1 : x2 : x3 : _) : cs ->
            reverseUnrolledMap cs (f x1 : f x2 : f x3 : acc)
          _ -> acc
      reverseUnrolledMap chunksAcc $ unrolledMap xs

instance functorWithIndexList :: FunctorWithIndex Int List where
  mapWithIndex f = foldrWithIndex (\i x acc -> f i x : acc) Nil

instance foldableList :: Foldable List where
  foldr f b = foldl (flip f) b <<< rev
    where
    rev = go Nil
      where
      go acc list = case list of
        Nil -> acc
        x : xs -> go (x : acc) xs
  foldl f = go
    where
    go b = case _ of
      Nil -> b
      a : as -> go (f b a) as
  foldMap f = foldl (\acc -> append acc <<< f) mempty

instance foldableWithIndexList :: FoldableWithIndex Int List where
  foldrWithIndex f b xs =
    -- as we climb the reversed list, we decrement the index
    snd $ foldl
      (\(Tuple i b') a -> Tuple (i - 1) (f (i - 1) a b'))
      (Tuple len b)
      revList
    where
    Tuple len revList = rev (Tuple 0 Nil) xs
      where
      -- As we create our reversed list, we count elements.
      rev = foldl (\(Tuple i acc) a -> Tuple (i + 1) (a : acc))
  foldlWithIndex f acc =
    snd <<< foldl (\(Tuple i b) a -> Tuple (i + 1) (f i b a)) (Tuple 0 acc)
  foldMapWithIndex f = foldlWithIndex (\i acc -> append acc <<< f i) mempty

instance unfoldable1List :: Unfoldable1 List where
  unfoldr1 f b = go b Nil
    where
    go source memo = case f source of
      Tuple one (Just rest) -> go rest (one : memo)
      Tuple one Nothing -> foldl (flip (:)) Nil (one : memo)

instance unfoldableList :: Unfoldable List where
  unfoldr f b = go b Nil
    where
    go source memo = case f source of
      Nothing -> (foldl (flip (:)) Nil memo)
      Just (Tuple one rest) -> go rest (one : memo)

instance traversableList :: Traversable List where
  traverse f = map (foldl (flip (:)) Nil) <<< foldl (\acc -> lift2 (flip (:)) acc <<< f) (pure Nil)
  sequence = traverse identity

instance traversableWithIndexList :: TraversableWithIndex Int List where
  traverseWithIndex f =
    map rev
      <<< foldlWithIndex (\i acc -> lift2 (flip (:)) acc <<< f i) (pure Nil)
    where
    rev = foldl (flip Cons) Nil

instance applyList :: Apply List where
  apply functions xs = case functions of
    Nil -> Nil
    f : fs -> (f <$> xs) <> (fs <*> xs)

instance applicativeList :: Applicative List where
  pure a = a : Nil

instance bindList :: Bind List where
  bind list f = case list of
    Nil -> Nil
    x : xs -> f x <> bind xs f

instance monadList :: Monad List

instance altList :: Alt List where
  alt = append

instance plusList :: Plus List where
  empty = Nil

instance alternativeList :: Alternative List

instance monadPlusList :: MonadPlus List

instance extendList :: Extend List where
  extend f list = case list of
    Nil -> Nil
    l@(_ : as) -> do
      let
        go a' { val, acc } = do
          let acc' = a' : acc
          { val: f acc' : val, acc: acc' }
      f l : (foldr go { val: Nil, acc: Nil } as).val

newtype NonEmptyList a = NonEmptyList (NonEmpty List a)

toList :: NonEmptyList ~> List
toList (NonEmptyList (x :| xs)) = x : xs

nelCons :: forall a. a -> NonEmptyList a -> NonEmptyList a
nelCons a (NonEmptyList (b :| bs)) = NonEmptyList (a :| b : bs)

derive instance newtypeNonEmptyList :: Newtype (NonEmptyList a) _

derive newtype instance eqNonEmptyList :: Eq a => Eq (NonEmptyList a)
derive newtype instance ordNonEmptyList :: Ord a => Ord (NonEmptyList a)

derive newtype instance eq1NonEmptyList :: Eq1 NonEmptyList
derive newtype instance ord1NonEmptyList :: Ord1 NonEmptyList

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

derive newtype instance foldable1NonEmptyList :: Foldable1 NonEmptyList

derive newtype instance unfoldable1NonEmptyList :: Unfoldable1 NonEmptyList

instance functorWithIndexNonEmptyList :: FunctorWithIndex Int NonEmptyList where
  mapWithIndex fn (NonEmptyList ne) = NonEmptyList $ mapWithIndex (fn <<< maybe 0 (add 1)) ne

instance foldableWithIndexNonEmptyList :: FoldableWithIndex Int NonEmptyList where
  foldMapWithIndex f (NonEmptyList ne) = foldMapWithIndex (f <<< maybe 0 (add 1)) ne
  foldlWithIndex f b (NonEmptyList ne) = foldlWithIndex (f <<< maybe 0 (add 1)) b ne
  foldrWithIndex f b (NonEmptyList ne) = foldrWithIndex (f <<< maybe 0 (add 1)) b ne

instance traversableWithIndexNonEmptyList :: TraversableWithIndex Int NonEmptyList where
  traverseWithIndex f (NonEmptyList ne) = NonEmptyList <$> traverseWithIndex (f <<< maybe 0 (add 1)) ne

instance traversable1NonEmptyList :: Traversable1 NonEmptyList where
  traverse1 f (NonEmptyList (a :| as)) =
    foldl (\acc -> lift2 (flip nelCons) acc <<< f) (pure <$> f a) as
      <#> case _ of NonEmptyList (x :| xs) -> foldl (flip nelCons) (pure x) xs
  sequence1 = traverse1 identity
