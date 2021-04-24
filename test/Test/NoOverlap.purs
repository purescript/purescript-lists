module Test.NoOverlap where

import Prelude

import Effect (Effect)

import Data.Foldable (class Foldable)
import Data.List as L
import Data.List.NonEmpty as NEL
import Data.List.Lazy as LL
import Data.List.Lazy.NonEmpty as LNEL
import Data.Maybe (Maybe(..))

import Test.Common (printTestType, makeContainer)

import Effect.Console (log)
import Test.Assert (assert)

{-
This file contains functions that cannot be tested generically.
-}


assertSkip :: (_ -> Boolean) -> Effect Unit
assertSkip _ =
  log "...skipped"

testOnlyStrictCanEmpty :: Effect Unit
testOnlyStrictCanEmpty = do

  let
    l :: forall f a. Foldable f => f a -> L.List a
    l = makeContainer

  printTestType "Only Strict canEmpty"

  log "deleteAt should remove an item at the specified index"
  assert $ L.deleteAt 0 (l [1, 2, 3]) == Just (l [2, 3])
  assert $ L.deleteAt 1 (l [1, 2, 3]) == Just (l [1, 3])


testOnlyStrictNonEmpty :: Effect Unit
testOnlyStrictNonEmpty = do

  let
    l :: forall f a. Foldable f => f a -> NEL.NonEmptyList a
    l = makeContainer

    cel :: forall f a. Foldable f => f a -> L.List a
    cel = makeContainer

  printTestType "Only Strict NonEmpty"

  log "deleteAt should remove an item at the specified index"
  assertSkip \_ -> NEL.deleteAt 0 (l [1, 2, 3]) == Just (cel [2, 3])
  assertSkip \_ -> NEL.deleteAt 1 (l [1, 2, 3]) == Just (cel [1, 3])


testOnlyLazyCanEmpty :: Effect Unit
testOnlyLazyCanEmpty = do

  let
    l :: forall f a. Foldable f => f a -> LL.List a
    l = makeContainer

  printTestType "Only Lazy canEmpty"

  log "deleteAt should remove an item at the specified index"
  assert $ LL.deleteAt 0 (l [1, 2, 3]) == l [2, 3]
  assert $ LL.deleteAt 1 (l [1, 2, 3]) == l [1, 3]


testOnlyLazyNonEmpty :: Effect Unit
testOnlyLazyNonEmpty = do

  let
    l :: forall f a. Foldable f => f a -> LNEL.NonEmptyList a
    l = makeContainer

    cel :: forall f a. Foldable f => f a -> LL.List a
    cel = makeContainer

  printTestType "Only Lazy NonEmpty"

  log "deleteAt should remove an item at the specified index"
  assert $ LNEL.deleteAt 0 (l [1, 2, 3]) == cel [2, 3]
  assert $ LNEL.deleteAt 1 (l [1, 2, 3]) == cel [1, 3]