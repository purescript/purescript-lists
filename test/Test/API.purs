module Test.API where

import Prelude

import Control.Alternative (class Alternative)
import Control.Lazy (class Lazy)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Unfoldable (class Unfoldable)

type Common c =
  { makeCollection :: forall f a. Foldable f => f a -> c a

  , concat :: forall a. c (c a) -> c a
  , concatMap :: forall a. forall b. (a -> c b) -> c a -> c b
  , cons :: forall a. a -> c a -> c a
  , elemIndex :: forall a. Eq a => a -> c a -> Maybe Int
  , elemLastIndex :: forall a. Eq a => a -> c a -> Maybe Int
  , findIndex :: forall a. (a -> Boolean) -> c a -> Maybe Int
  , findLastIndex :: forall a. (a -> Boolean) -> c a -> Maybe Int
  , foldM :: forall m a b. Monad m => (b -> a -> m b) -> b -> c a -> m b
  , index :: forall a. c a -> Int -> Maybe a
  , intersect :: forall a. Eq a => c a -> c a -> c a
  , intersectBy :: forall a. (a -> a -> Boolean) -> c a -> c a -> c a
  , length :: forall a. c a -> Int
  , nubEq :: forall a. Eq a => c a -> c a
  , nubByEq :: forall a. (a -> a -> Boolean) -> c a -> c a
  , range :: Int -> Int -> c Int
  , reverse :: c ~> c
  , singleton :: forall a. a -> c a
  , snoc :: forall a. c a -> a -> c a
  , toUnfoldable :: forall f a. Unfoldable f => c a -> f a
  , union :: forall a. Eq a => c a -> c a -> c a
  , unionBy :: forall a. (a -> a -> Boolean) -> c a -> c a -> c a
  , unzip :: forall a b. c (Tuple a b) -> Tuple (c a) (c b)
  , zip :: forall a b. c a -> c b -> c (Tuple a b)
  , zipWith :: forall a b d. (a -> b -> d) -> c a -> c b -> c d
  , zipWithA :: forall a b d m. Applicative m => (a -> b -> m d) -> c a -> c b -> m (c d)

  , appendFoldable :: forall t a. Foldable t => c a -> t a -> c a
  , insert :: forall a. Ord a => a -> c a -> c a
  , insertBy :: forall a. (a -> a -> Ordering) -> a -> c a -> c a
  , nub :: forall a. Ord a => c a -> c a
  , nubBy :: forall a. (a -> a -> Ordering) -> c a -> c a
  -- , replicate :: forall a. Int -> a -> c a
  , some :: forall f a. Alternative f => Lazy (f (c a)) => f a -> f (c a)
  , someRec :: forall f a. MonadRec f => Alternative f => f a -> f (c a)
  , sort :: forall a. Ord a => c a -> c a
  , sortBy :: forall a. (a -> a -> Ordering) -> c a -> c a
  , transpose :: forall a. c (c a) -> c (c a)
  }

type CommonDiffEmptiability c cInverse canEmpty nonEmpty cPattern =
  { makeCollection :: forall f a. Foldable f => f a -> c a

  , makeCanEmptyCollection :: forall f a. Foldable f => f a -> canEmpty a
  , makeNonEmptyCollection :: forall f a. Foldable f => f a -> nonEmpty a

  , catMaybes :: forall a. c (Maybe a) -> canEmpty a
  , drop :: forall a. Int -> c a -> canEmpty a
  , dropWhile :: forall a. (a -> Boolean) -> c a -> canEmpty a
  , filter :: forall a. (a -> Boolean) -> c a -> canEmpty a
  , filterM :: forall m a. Monad m => (a -> m Boolean) -> c a -> m (canEmpty a)
  , group :: forall a. Eq a => c a -> c (nonEmpty a)
  , groupAll :: forall a. Ord a => c a -> c (nonEmpty a)
  , groupBy :: forall a. (a -> a -> Boolean) -> c a -> c (nonEmpty a)
  , mapMaybe :: forall a b. (a -> Maybe b) -> c a -> canEmpty b
  , partition :: forall a. (a -> Boolean) -> c a -> { no :: canEmpty a, yes :: canEmpty a }
  , span :: forall a. (a -> Boolean) -> c a -> { init :: canEmpty a, rest :: canEmpty a }
  , take :: forall a. Int -> c a -> canEmpty a
  , takeEnd :: forall a. Int -> c a -> canEmpty a
  , takeWhile :: forall a. (a -> Boolean) -> c a -> canEmpty a

  , cons' :: forall a. a -> cInverse a -> c a
  , delete :: forall a. Eq a => a -> c a -> canEmpty a
  , deleteBy :: forall a. (a -> a -> Boolean) -> a -> c a -> canEmpty a
  , difference :: forall a. Eq a => c a -> c a -> canEmpty a
  , dropEnd :: forall a. Int -> c a -> canEmpty a
  , groupAllBy :: forall a. (a -> a -> Ordering) -> c a -> c (nonEmpty a)
  , pattern :: forall a. c a -> cPattern a
  , slice :: Int -> Int -> c ~> canEmpty
  , snoc' :: forall a. cInverse a -> a -> c a
  , stripPrefix :: forall a. Eq a => cPattern a -> c a -> Maybe (canEmpty a)
}

type OnlyCanEmpty c nonEmpty =
  { makeCollection :: forall f a. Foldable f => f a -> c a
  , makeNonEmptyCollection :: forall f a. Foldable f => f a -> nonEmpty a

  -- These are the same function names as the NonEmpty versions,
  -- but the signatures are different and can't be merged in the
  -- CommonDiffEmptiability tests. This is due to a mismatch in the
  -- presence of `Maybe`s.
  , fromFoldable :: forall f. Foldable f => f ~> c
  , head :: forall a. c a -> Maybe a
  , init :: forall a. c a -> Maybe (c a)
  , last :: forall a. c a -> Maybe a
  , tail :: forall a. c a -> Maybe (c a)
  , uncons :: forall a. c a -> Maybe { head :: a, tail :: c a }

  -- These are not available for non-empty collections
  , null :: forall a. c a -> Boolean
  , many :: forall f a. Alternative f => Lazy (f (c a)) => f a -> f (c a)
  , manyRec :: forall f a. MonadRec f => Alternative f => f a -> f (c a)
  }

type OnlyNonEmpty c canEmpty =
  { makeCollection :: forall f a. Foldable f => f a -> c a
  , makeCanEmptyCollection :: forall f a. Foldable f => f a -> canEmpty a

  -- These are the same function names as the CanEmpty versions,
  -- but the signatures are different and can't be merged in the
  -- CommonDiffEmptiability tests. This is due to a mismatch in the
  -- presence of `Maybe`s.

  , fromFoldable :: forall f a. Foldable f => f a -> Maybe (c a)
  , head :: forall a. c a -> a
  , init :: forall a. c a -> canEmpty a
  , last :: forall a. c a -> a
  , tail :: forall a. c a -> canEmpty a
  , uncons :: forall a. c a -> { head :: a, tail :: canEmpty a }

  -- These are only available for NonEmpty collections

  , fromList :: forall a. canEmpty a -> Maybe (c a)
  , toList :: c ~> canEmpty
  }

type OnlyStrict c =
  { makeCollection :: forall f a. Foldable f => f a -> c a

  -- Same names, but different APIs (with Maybe)
  , alterAt :: forall a. Int -> (a -> Maybe a) -> c a -> Maybe (c a)
  , insertAt :: forall a. Int -> a -> c a -> Maybe (c a)
  , modifyAt :: forall a. Int -> (a -> a) -> c a -> Maybe (c a)
  , updateAt :: forall a. Int -> a -> c a -> Maybe (c a)
  }

type OnlyLazy c =
  { makeCollection :: forall f a. Foldable f => f a -> c a

  -- Same names, but different APIs (without Maybe)
  , alterAt :: forall a. Int -> (a -> Maybe a) -> c a -> c a
  , insertAt :: forall a. Int -> a -> c a -> c a
  , modifyAt :: forall a. Int -> (a -> a) -> c a -> c a
  , updateAt :: forall a. Int -> a -> c a -> c a

  -- These are only available for Lazy collections
  , iterate :: forall a. (a -> a) -> a -> c a
  , repeat :: forall a. a -> c a
  , cycle :: forall a. c a -> c a
  , foldrLazy :: forall a b. Lazy b => (a -> b -> b) -> b -> c a -> b
  , scanlLazy :: forall a b. (b -> a -> b) -> b -> c a -> c b

  -- Specialized from Unfoldable1's replicate1 / replicate1A
  , replicate1 :: forall a. Int -> a -> c a
  , replicate1M :: forall m a. Monad m => Int -> m a -> m (c a)
  }


-- Non Overlapping APIs

type OnlyStrictCanEmpty :: forall k. (k -> Type) -> Type
type OnlyStrictCanEmpty c =
  {
  -- Same names, but different APIs
    deleteAt :: forall a. Int -> c a -> Maybe (c a)
  }

type OnlyStrictNonEmpty :: forall k. (k -> Type) -> (k -> Type) -> Type
type OnlyStrictNonEmpty c canEmpty =
  {
  -- Same names, but different APIs
    deleteAt :: forall a. Int -> c a -> Maybe (canEmpty a)
  }

-- Todo - investigate why kind signature is only recommended when
-- records contain only a single field

type OnlyLazyCanEmpty c =
  {
  -- Same names, but different APIs
    deleteAt :: forall a. Int -> c a -> c a
  -- Unique functions
  -- Specialized from Unfoldable's replicate / replicateA
  , replicate :: forall a. Int -> a -> c a
  , replicateM :: forall m a. Monad m => Int -> m a -> m (c a)
  }

type OnlyLazyNonEmpty :: forall k. (k -> Type) -> (k -> Type) -> Type
type OnlyLazyNonEmpty c canEmpty =
  {
  -- Same names, but different APIs
    deleteAt :: forall a. Int -> c a -> canEmpty a
  }