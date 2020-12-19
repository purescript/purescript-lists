module Test.OnlyCanEmpty where

import Prelude

import Control.Alternative (class Alternative)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus, empty)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert)

import Test.Common (class Common, SkipBroken(..), assertSkipHelper, printTestType, makeContainer, range)

import Data.List as L
import Data.List.NonEmpty as NEL
import Data.List.Lazy as LL
import Data.List.Lazy.NonEmpty as LNEL

class (
  Alternative c
  , MonadPlus c
  , MonadZero c
  , Monoid (c Int) -- Monoid1?
  , Plus c
  , Unfoldable c
) <= OnlyCanEmpty c nonEmpty | c -> nonEmpty, nonEmpty -> c where

  makeNonEmptyContainer :: forall f a. Foldable f => f a -> nonEmpty a

  -- These are the same function names as the NonEmpty versions,
  -- but the signatures are different and can't be merged in the
  -- CommonDiffEmptiability tests. This is due to a mismatch in the
  -- presence of `Maybe`s.
  fromFoldable :: forall f. Foldable f => f ~> c
  head :: forall a. c a -> Maybe a
  init :: forall a. c a -> Maybe (c a)
  last :: forall a. c a -> Maybe a
  tail :: forall a. c a -> Maybe (c a)
  uncons :: forall a. c a -> Maybe { head :: a, tail :: c a }

instance onlyCanEmptyList :: OnlyCanEmpty L.List NEL.NonEmptyList where

  makeNonEmptyContainer = unsafePartial fromJust <<< NEL.fromFoldable

  fromFoldable = L.fromFoldable
  head = L.head
  init = L.init
  last = L.last
  tail = L.tail
  uncons = L.uncons

instance onlyCanEmptyLazyList :: OnlyCanEmpty LL.List LNEL.NonEmptyList where

  makeNonEmptyContainer = unsafePartial fromJust <<< LNEL.fromFoldable

  fromFoldable = LL.fromFoldable
  head = LL.head
  init = LL.init
  last = LL.last
  tail = LL.tail
  uncons = LL.uncons


testOnlyCanEmpty :: forall c nonEmpty.
  Common c =>
  OnlyCanEmpty c nonEmpty =>
  Eq (c Int) =>
  Eq (c (nonEmpty Int)) =>
  c Int -> nonEmpty Int -> Effect Unit
testOnlyCanEmpty nil _ = do
  let
    l :: forall f a. Foldable f => f a -> c a
    l = makeContainer

    nel :: forall f a. Foldable f => f a -> nonEmpty a
    nel = makeNonEmptyContainer

    rg :: Int -> Int -> c Int
    rg = range

  printTestType "Only canEmpty"

  -- ======= Typeclass tests ========

  -- Alternative
  -- applicative and plus
  -- (f <|> g) <*> x == (f <*> x) <|> (g <*> x)
  -- empty <*> f == empty

  -- MonadPlus
  -- Additional law on MonadZero
  -- (x <|> y) >>= f == (x >>= f) <|> (y >>= f)

  -- MonadZero
  -- monad and alternative
  -- empty >>= f = empty

  -- Monoid
  -- mempty :: c
  log "mempty should not change the container it is appended to"
  assert $ l [5] <> mempty == l [5]
  log "mempty should be an empty container"
  assert $ l [] == (mempty :: c Int)

  -- Plus
  -- empty :: forall a. c a
  log "empty should create an empty container"
  assert $ l [] == (empty :: c Int)

  -- Unfoldable
  -- unfoldr :: forall a b. (b -> Maybe (Tuple a b)) -> b -> c a

  log "unfoldr should maintain order"
  let
    step :: Int -> Maybe (Tuple Int Int)
    step 6 = Nothing
    step n = Just (Tuple n (n + 1))
  assert $ rg 1 5 == unfoldr step 1


  -- ======= Functions tests ========

  --fromFoldable :: forall f. Foldable f => f ~> c
  --already extensively checked in common tests

  -- These are the remaining functions that can't be deduplicated due to use of Maybe

  -- Todo - double-check the phrasing on these? Might be confusing to refer to a
  -- non-empty canEmpty list.

  log "head should return a Just-NEL.NonEmptyListped first value of a non-empty list"
  assert $ head (l [1, 2]) == Just 1

  log "head should return Nothing for an empty list"
  assert $ head nil == Nothing

  -- Todo - phrasing should be changed to note all but last (not all but first).
  log "init should return a Just-NEL.NonEmptyListped list containing all the items in an list apart from the first for a non-empty list"
  assert $ init (l [1, 2, 3]) == Just (l [1, 2])

  log "init should return Nothing for an empty list"
  assert $ init nil == Nothing


  log "last should return a Just-NEL.NonEmptyListped last value of a non-empty list"
  assert $ last (l [1, 2]) == Just 2

  log "last should return Nothing for an empty list"
  assert $ last nil == Nothing


  log "tail should return a Just-NEL.NonEmptyListped list containing all the items in an list apart from the first for a non-empty list"
  assert $ tail (l [1, 2, 3]) == Just (l [2, 3])

  log "tail should return Nothing for an empty list"
  assert $ tail nil == Nothing


  log "uncons should return nothing when used on an empty list"
  assert $ isNothing (uncons nil)

  log "uncons should split an list into a head and tail record when there is at least one item"
  assert $ uncons (l [1]) == Just {head: 1, tail: l []}
  assert $ uncons (l [1, 2, 3]) == Just {head: 1, tail: l [2, 3]}
