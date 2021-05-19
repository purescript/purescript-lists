module Data.List.Lazy.NonEmpty
  ( module Data.List.Lazy.Types
  , toUnfoldable
  , fromFoldable
  , fromList
  , toList
  , singleton
  , repeat
  , iterate
  , head
  , last
  , tail
  , init
  , uncons
  , length
  , concat
  , concatMap
  , appendFoldable
  -- additions
  , catMaybes
  , cons
  , drop
  , dropWhile
  , elemIndex
  , elemLastIndex
  , filter
  , filterM
  , findIndex
  , findLastIndex
  , foldM
  , group
  , groupAll
  , groupBy
  , index
  , insertAt
  , intersect
  , intersectBy
  , mapMaybe
  , modifyAt
  , nubEq
  , nubByEq
  , partition
  , range
  , reverse
  , snoc
  , snoc'
  , span
  , take
  , takeEnd
  , takeWhile
  , union
  , unionBy
  , unzip
  , updateAt
  , zip
  , zipWith
  , zipWithA

  , insert
  , insertBy
  , nub
  , nubBy
  , Pattern(..)
  , replicate
  , replicateM
  , some
  , someRec
  , sort
  , sortBy
  , transpose

  , cons'
  , delete
  , deleteBy
  , difference
  , dropEnd
  , groupAllBy
  , slice
  , stripPrefix
  , deleteAt
  , alterAt

  , cycle
  , foldrLazy
  , scanlLazy

  , replicate1
  , replicate1M

  ) where

import Prelude

import Control.Alternative (class Alternative)
import Control.Lazy (class Lazy)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Foldable (class Foldable)
import Data.Lazy (force, defer)
import Data.List.Lazy ((:))
import Data.List.Lazy as L
import Data.List.Lazy.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Partial.Unsafe (unsafeCrashWith)

--- Sorted additions ------

-- | Filter a list of optional values, keeping only the elements which contain
-- | a value.
catMaybes :: forall a. NonEmptyList (Maybe a) -> L.List a
catMaybes _ = unsafeCrashWith "todo catMaybes for Lazy NonEmptyList"
--catMaybes = mapMaybe identity

cons :: forall a. a -> NonEmptyList a -> NonEmptyList a
cons _ _ = unsafeCrashWith "todo cons for Lazy NonEmptyList"

-- | Drop the specified number of elements from the front of a list.
drop :: forall a. Int -> NonEmptyList a -> L.List a
drop _ _ = unsafeCrashWith "todo drop for Lazy NonEmptyList"

dropWhile :: forall a. (a -> Boolean) -> NonEmptyList a -> L.List a
dropWhile _ _ = unsafeCrashWith "todo dropWhile for Lazy NonEmptyList"

elemIndex :: forall a. Eq a => a -> NonEmptyList a -> Maybe Int
elemIndex _ _ = unsafeCrashWith "todo elemIndex for Lazy NonEmptyList"

elemLastIndex :: forall a. Eq a => a -> NonEmptyList a -> Maybe Int
elemLastIndex _ _ = unsafeCrashWith "todo elemLastIndex for Lazy NonEmptyList"

filter :: forall a. (a -> Boolean) -> NonEmptyList a -> L.List a
filter _ _ = unsafeCrashWith "todo filter for Lazy NonEmptyList"

filterM :: forall m a. Monad m => (a -> m Boolean) -> NonEmptyList a -> m (L.List a)
filterM _ _ = unsafeCrashWith "todo filterM for Lazy NonEmptyList"

findIndex :: forall a. (a -> Boolean) -> NonEmptyList a -> Maybe Int
findIndex _ _ = unsafeCrashWith "todo findIndex for Lazy NonEmptyList"

findLastIndex :: forall a. (a -> Boolean) -> NonEmptyList a -> Maybe Int
findLastIndex _ _ = unsafeCrashWith "todo findLastIndex for Lazy NonEmptyList"

foldM :: forall m a b. Monad m => (b -> a -> m b) -> b -> NonEmptyList a -> m b
foldM _ _ _ = unsafeCrashWith "todo foldM for Lazy NonEmptyList"

group :: forall a. Eq a => NonEmptyList a -> NonEmptyList (NonEmptyList a)
group _ = unsafeCrashWith "todo group for Lazy NonEmptyList"

groupAll :: forall a. Ord a => NonEmptyList a -> NonEmptyList (NonEmptyList a)
groupAll _ = unsafeCrashWith "todo groupAll for Lazy NonEmptyList"

groupBy :: forall a. (a -> a -> Boolean) -> NonEmptyList a -> NonEmptyList (NonEmptyList a)
groupBy _ _ = unsafeCrashWith "todo groupBy for Lazy NonEmptyList"

index :: forall a. NonEmptyList a -> Int -> Maybe a
index _ _ = unsafeCrashWith "todo index for Lazy NonEmptyList"

insertAt :: forall a. Int -> a -> NonEmptyList a -> NonEmptyList a
insertAt _ _ _ = unsafeCrashWith "todo insertAt for Lazy NonEmptyList"

intersect :: forall a. Eq a => NonEmptyList a -> NonEmptyList a -> NonEmptyList a
intersect _ _ = unsafeCrashWith "todo intersect for Lazy NonEmptyList"

intersectBy :: forall a. (a -> a -> Boolean) -> NonEmptyList a -> NonEmptyList a -> NonEmptyList a
intersectBy _ _ _ = unsafeCrashWith "todo intersectBy for Lazy NonEmptyList"

mapMaybe :: forall a b. (a -> Maybe b) -> NonEmptyList a -> L.List b
mapMaybe _ _ = unsafeCrashWith "todo mapMaybe for Lazy NonEmptyList"

modifyAt :: forall a. Int -> (a -> a) -> NonEmptyList a -> NonEmptyList a
modifyAt _ _ _ = unsafeCrashWith "todo modifyAt for Lazy NonEmptyList"

nubEq :: forall a. Eq a => NonEmptyList a -> NonEmptyList a
nubEq _ = unsafeCrashWith "todo nubEq for Lazy NonEmptyList"

nubByEq :: forall a. (a -> a -> Boolean) -> NonEmptyList a -> NonEmptyList a
nubByEq _ _ = unsafeCrashWith "todo nubByEq for Lazy NonEmptyList"

partition :: forall a. (a -> Boolean) -> NonEmptyList a -> { yes :: L.List a, no :: L.List a }
partition _ _ = unsafeCrashWith "todo partition for Lazy NonEmptyList"
range :: Int -> Int -> NonEmptyList Int
range _ _ = unsafeCrashWith "todo range for Lazy NonEmptyList"

reverse :: forall a. NonEmptyList a -> NonEmptyList a
reverse _ = unsafeCrashWith "todo reverse for Lazy NonEmptyList"

snoc :: forall a. NonEmptyList a -> a -> NonEmptyList a
snoc _ _ = unsafeCrashWith "todo snoc for Lazy NonEmptyList"

snoc' :: forall a. L.List a -> a -> NonEmptyList a
snoc' _ _ = unsafeCrashWith "todo snoc' for Lazy NonEmptyList"

span :: forall a. (a -> Boolean) -> NonEmptyList a -> { init :: L.List a, rest :: L.List a }
span _ _ = unsafeCrashWith "todo span for Lazy NonEmptyList"

take :: forall a. Int -> NonEmptyList a -> L.List a
take _ _ = unsafeCrashWith "todo take for Lazy NonEmptyList"

takeEnd :: forall a. Int -> NonEmptyList a -> L.List a
takeEnd _ _ = unsafeCrashWith "todo takeEnd for Lazy NonEmptyList"

takeWhile :: forall a. (a -> Boolean) -> NonEmptyList a -> L.List a
takeWhile _ _ = unsafeCrashWith "todo takeWhile for Lazy NonEmptyList"

union :: forall a. Eq a => NonEmptyList a -> NonEmptyList a -> NonEmptyList a
union _ _ = unsafeCrashWith "todo union for Lazy NonEmptyList"

unionBy :: forall a. (a -> a -> Boolean) -> NonEmptyList a -> NonEmptyList a -> NonEmptyList a
unionBy _ _ _ = unsafeCrashWith "todo unionBy for Lazy NonEmptyList"

unzip :: forall a b. NonEmptyList (Tuple a b) -> Tuple (NonEmptyList a) (NonEmptyList b)
unzip _ = unsafeCrashWith "todo unzip for Lazy NonEmptyList"

updateAt :: forall a. Int -> a -> NonEmptyList a -> NonEmptyList a
updateAt _ _ _ = unsafeCrashWith "todo updateAt for Lazy NonEmptyList"

zip :: forall a b. NonEmptyList a -> NonEmptyList b -> NonEmptyList (Tuple a b)
zip _ _ = unsafeCrashWith "todo zip for Lazy NonEmptyList"

zipWith :: forall a b c. (a -> b -> c) -> NonEmptyList a -> NonEmptyList b -> NonEmptyList c
zipWith _ _ _ = unsafeCrashWith "todo zipWith for Lazy NonEmptyList"

zipWithA :: forall m a b c. Applicative m => (a -> b -> m c) -> NonEmptyList a -> NonEmptyList b -> m (NonEmptyList c)
zipWithA _ _ _ = unsafeCrashWith "todo zipWithA for Lazy NonEmptyList"


insert :: forall a. Ord a => a -> NonEmptyList a -> NonEmptyList a
insert _ _ = unsafeCrashWith "todo insert for Lazy NonEmptyList"
insertBy :: forall a. (a -> a -> Ordering) -> a -> NonEmptyList a -> NonEmptyList a
insertBy _ _ _ = unsafeCrashWith "todo insertBy for Lazy NonEmptyList"
nub :: forall a. Ord a => NonEmptyList a -> NonEmptyList a
nub _ = unsafeCrashWith "todo nub for Lazy NonEmptyList"
nubBy :: forall a. (a -> a -> Ordering) -> NonEmptyList a -> NonEmptyList a
nubBy _ _ = unsafeCrashWith "todo nubBy for Lazy NonEmptyList"
replicate :: forall a. Int -> a -> NonEmptyList a
replicate _ _ = unsafeCrashWith "todo replicate for Lazy NonEmptyList"
replicateM :: forall m a. Monad m => Int -> m a -> m (NonEmptyList a)
replicateM _ _ = unsafeCrashWith "todo replicateM for Lazy NonEmptyList"
some :: forall f a. Alternative f => Lazy (f (NonEmptyList a)) => f a -> f (NonEmptyList a)
some _ = unsafeCrashWith "todo some for Lazy NonEmptyList"
someRec :: forall f a. MonadRec f => Alternative f => f a -> f (NonEmptyList a)
someRec _ = unsafeCrashWith "todo someRec for Lazy NonEmptyList"
sort :: forall a. Ord a => NonEmptyList a -> NonEmptyList a
sort _ = unsafeCrashWith "todo sort for Lazy NonEmptyList"
sortBy :: forall a. (a -> a -> Ordering) -> NonEmptyList a -> NonEmptyList a
sortBy _ _ = unsafeCrashWith "todo sortBy for Lazy NonEmptyList"
transpose :: forall a. NonEmptyList (NonEmptyList a) -> NonEmptyList (NonEmptyList a)
transpose _ = unsafeCrashWith "todo transpose for Lazy NonEmptyList"

cons' :: forall a. a -> L.List a -> NonEmptyList a
cons' _ _ = unsafeCrashWith "todo cons' for LazyNonEmptyList"
delete :: forall a. Eq a => a -> NonEmptyList a -> L.List a
delete _ _ = unsafeCrashWith "todo delete for LazyNonEmptyList"
deleteBy :: forall a. (a -> a -> Boolean) -> a -> NonEmptyList a -> L.List a
deleteBy _ _ _ = unsafeCrashWith "todo deleteBy for LazyNonEmptyList"
difference :: forall a. Eq a => NonEmptyList a -> NonEmptyList a -> L.List a
difference _ _ = unsafeCrashWith "todo difference for LazyNonEmptyList"
dropEnd :: forall a. Int -> NonEmptyList a -> L.List a
dropEnd _ _ = unsafeCrashWith "todo dropEnd for LazyNonEmptyList"
groupAllBy :: forall a. (a -> a -> Ordering) -> NonEmptyList a -> NonEmptyList (NonEmptyList a)
groupAllBy _ _ = unsafeCrashWith "todo groupAllBy for LazyNonEmptyList"
slice :: Int -> Int -> NonEmptyList ~> L.List
slice _ _ = unsafeCrashWith "todo slice for LazyNonEmptyList"
stripPrefix :: forall a. Eq a => Pattern a -> NonEmptyList a -> Maybe (L.List a)
stripPrefix _ _ = unsafeCrashWith "todo stripPrefix for LazyNonEmptyList"

deleteAt :: forall a. Int -> NonEmptyList a -> L.List a
deleteAt _ _ = unsafeCrashWith "todo deleteAt for LazyNonEmptyList"

alterAt :: forall a. Int -> (a -> Maybe a) -> NonEmptyList a -> NonEmptyList a
alterAt _ _ _ = unsafeCrashWith "todo alterAt for LazyNonEmptyList"

cycle :: forall a. NonEmptyList a -> NonEmptyList a
cycle _ = unsafeCrashWith "todo cycle for LazyNonEmptyList"
foldrLazy :: forall a b. Lazy b => (a -> b -> b) -> b -> NonEmptyList a -> b
foldrLazy _ _ _ = unsafeCrashWith "todo foldrLazy for LazyNonEmptyList"
scanlLazy :: forall a b. (b -> a -> b) -> b -> NonEmptyList a -> NonEmptyList b
scanlLazy _ _ _ = unsafeCrashWith "todo scanlLazy for LazyNonEmptyList"

-- Specialized from Unfoldable1's replicate1 / replicate1A
replicate1 :: forall a. Int -> a -> NonEmptyList a
replicate1 _ _ = unsafeCrashWith "todo replicate1 for LazyNonEmptyList"

replicate1M :: forall m a. Monad m => Int -> m a -> m (NonEmptyList a)
replicate1M _ _ = unsafeCrashWith "todo replicate1M for LazyNonEmptyList"

-----------

toUnfoldable :: forall f. Unfoldable f => NonEmptyList ~> f
toUnfoldable =
  unfoldr (\xs -> (\rec -> Tuple rec.head rec.tail) <$> L.uncons xs) <<< toList

fromFoldable :: forall f a. Foldable f => f a -> Maybe (NonEmptyList a)
fromFoldable = fromList <<< L.fromFoldable

fromList :: forall a. L.List a -> Maybe (NonEmptyList a)
fromList l =
  case L.step l of
    L.Nil -> Nothing
    L.Cons x xs -> Just (NonEmptyList (defer \_ -> x :| xs))

toList :: NonEmptyList ~> L.List
toList (NonEmptyList nel) = case force nel of x :| xs -> x : xs

singleton :: forall a. a -> NonEmptyList a
singleton = pure

repeat :: forall a. a -> NonEmptyList a
repeat x = NonEmptyList $ defer \_ -> x :| L.repeat x

iterate :: forall a. (a -> a) -> a -> NonEmptyList a
iterate f x = NonEmptyList $ defer \_ -> x :| L.iterate f (f x)

head :: forall a. NonEmptyList a -> a
head (NonEmptyList nel) = case force nel of x :| _ -> x

last :: forall a. NonEmptyList a -> a
last (NonEmptyList nel) = case force nel of x :| xs -> fromMaybe x (L.last xs)

tail :: NonEmptyList ~> L.List
tail (NonEmptyList nel) = case force nel of _ :| xs -> xs

init :: NonEmptyList ~> L.List
init (NonEmptyList nel) =
  case force nel
    of x :| xs ->
      maybe L.nil (x : _) (L.init xs)

uncons :: forall a. NonEmptyList a -> { head :: a, tail :: L.List a }
uncons (NonEmptyList nel) = case force nel of x :| xs -> { head: x, tail: xs }

length :: forall a. NonEmptyList a -> Int
length (NonEmptyList nel) = case force nel of _ :| xs -> 1 + L.length xs

-- | Flatten a list of lists.
-- |
-- | Running time: `O(n)`, where `n` is the total number of elements.
concat :: forall a. NonEmptyList (NonEmptyList a) -> NonEmptyList a
concat = (_ >>= identity)

concatMap :: forall a b. (a -> NonEmptyList b) -> NonEmptyList a -> NonEmptyList b
concatMap = flip bind

appendFoldable :: forall t a. Foldable t => NonEmptyList a -> t a -> NonEmptyList a
appendFoldable nel ys =
  NonEmptyList (defer \_ -> head nel :| tail nel <> L.fromFoldable ys)

-- | A newtype used in cases where there is a list to be matched.
newtype Pattern a = Pattern (NonEmptyList a)

derive instance eqPattern :: Eq a => Eq (Pattern a)
derive instance ordPattern :: Ord a => Ord (Pattern a)
derive instance newtypePattern :: Newtype (Pattern a) _

instance showPattern :: Show a => Show (Pattern a) where
  show (Pattern s) = "(Pattern " <> show s <> ")"
