module Test.OnlyLazy where

import Prelude

import Data.Foldable (class Foldable)
import Control.Lazy (class Lazy)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert)

import Test.Common (class Common, SkipBroken(..), assertSkipHelper, printTestType, makeContainer)

import Data.List.Lazy as LL
import Data.List.Lazy.NonEmpty as LNEL

{-
class (
  Lazy (c Int) -- missing from LazyNonEmptyList
) <= OnlyLazy c where
-}

class OnlyLazy c where

-- Same names, but different APIs (without Maybe)
  insertAt :: forall a. Int -> a -> c a -> c a
  modifyAt :: forall a. Int -> (a -> a) -> c a -> c a
  updateAt :: forall a. Int -> a -> c a -> c a

instance onlyLazyList :: OnlyLazy LL.List where
  insertAt = LL.insertAt
  modifyAt = LL.modifyAt
  updateAt = LL.updateAt

instance onlyLazyNonEmptyList :: OnlyLazy LNEL.NonEmptyList where
  insertAt = LNEL.insertAt
  modifyAt = LNEL.modifyAt
  updateAt = LNEL.updateAt

testOnlyLazy :: forall c.
  Common c =>
  OnlyLazy c =>
  c Int -> Effect Unit
testOnlyLazy _ = do
  let
    l :: forall f a. Foldable f => f a -> c a
    l = makeContainer

  printTestType "Only Lazy"

  log "insertAt should add an item at the specified index"
  assert $ (insertAt 0 1 (l [2, 3])) == (l [1, 2, 3])
  assert $ (insertAt 1 1 (l [2, 3])) == (l [2, 1, 3])
  assert $ (insertAt 2 1 (l [2, 3])) == (l [2, 3, 1])

  log "modifyAt should update an item at the specified index"
  assert $ (modifyAt 0 (_ + 1) (l [1, 2, 3])) == (l [2, 2, 3])
  assert $ (modifyAt 1 (_ + 1) (l [1, 2, 3])) == (l [1, 3, 3])

  log "updateAt should replace an item at the specified index"
  assert $ (updateAt 0 9 (l [1, 2, 3])) == (l [9, 2, 3])
  assert $ (updateAt 1 9 (l [1, 2, 3])) == (l [1, 9, 3])

