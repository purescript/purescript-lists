module Test.OnlyStrict where

import Prelude

import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert)

import Test.Common (class Common, SkipBroken(..), assertSkipHelper, printTestType, makeCollection)

import Data.List as L
import Data.List.NonEmpty as NEL

class OnlyStrict c where
-- Potentially just these functions:
-- Seems like they could also be common
{-
group'
mapWithIndex
sort
sortBy
unsnoc
-}

  -- Same names, but different APIs (with Maybe)
  alterAt :: forall a. Int -> (a -> Maybe a) -> c a -> Maybe (c a)
  insertAt :: forall a. Int -> a -> c a -> Maybe (c a)
  modifyAt :: forall a. Int -> (a -> a) -> c a -> Maybe (c a)
  updateAt :: forall a. Int -> a -> c a -> Maybe (c a)

  -- Strict only
  -- recently fixed, so now common
  --nub :: forall a. Ord a => c a -> c a
  --nubBy :: forall a. (a -> a -> Ordering) -> c a -> c a

instance onlyStrictList :: OnlyStrict L.List where
  alterAt = L.alterAt
  insertAt = L.insertAt
  modifyAt = L.modifyAt
  updateAt = L.updateAt

instance onlyStrictNonEmptyList :: OnlyStrict NEL.NonEmptyList where
  alterAt = NEL.alterAt
  insertAt = NEL.insertAt
  modifyAt = NEL.modifyAt
  updateAt = NEL.updateAt



testOnlyStrict :: forall c.
  Common c =>
  OnlyStrict c =>
  c Int -> Effect Unit
testOnlyStrict _ = do

  let
    l :: forall f a. Foldable f => f a -> c a
    l = makeCollection

  printTestType "Only Strict"

  -- todo insertAt test
  -- missing from original test suite

  -- todo modifyAt test
  -- missing from original test suite

  log "updateAt should replace an item at the specified index"
  assert $ (updateAt 0 9 (l [1, 2, 3])) == Just (l [9, 2, 3])
  assert $ (updateAt 1 9 (l [1, 2, 3])) == Just (l [1, 9, 3])

  log "updateAt should return Nothing if the index is out of range"
  assert $ (updateAt 5 9 (l [1, 2, 3])) == Nothing




