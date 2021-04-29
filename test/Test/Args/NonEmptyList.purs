module Test.Args.NonEmptyList where

import Data.List.NonEmpty

import Data.Foldable (class Foldable)
import Data.List as L
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude ((<<<))
import Test.API (Common, CommonDiffEmptiability, OnlyNonEmpty, OnlyStrict)

makeCollection :: forall a f. Foldable f => f a -> NonEmptyList a
makeCollection = unsafePartial fromJust <<< fromFoldable

makeCanEmptyCollection :: forall a f. Foldable f => f a -> L.List a
makeCanEmptyCollection = L.fromFoldable

makeNonEmptyCollection :: forall a f. Foldable f => f a -> NonEmptyList a
makeNonEmptyCollection = makeCollection

common :: Common NonEmptyList
common =
  { makeCollection

  , concat
  , concatMap
  , cons
  , elemIndex
  , elemLastIndex
  , findIndex
  , findLastIndex
  , foldM
  , index
  , intersect
  , intersectBy
  , length
  , nubEq
  , nubByEq
  , range
  , reverse
  , singleton
  , snoc
  , toUnfoldable
  , union
  , unionBy
  , unzip
  , zip
  , zipWith
  , zipWithA

  , appendFoldable
  , insert
  , insertBy
  , nub
  , nubBy
  , some
  , someRec
  , sort
  , sortBy
  , transpose
  }

commonDiffEmptiability :: CommonDiffEmptiability NonEmptyList L.List L.List NonEmptyList Pattern
commonDiffEmptiability =
  { makeCollection
  , makeCanEmptyCollection
  , makeNonEmptyCollection

  , catMaybes
  , drop
  , dropWhile
  , filter
  , filterM
  , group
  , groupAll
  , groupBy
  , mapMaybe
  , partition
  , span
  , take
  , takeEnd
  , takeWhile

  , cons'
  , delete
  , deleteBy
  , difference
  , dropEnd
  , groupAllBy
  , pattern: Pattern
  , slice
  , snoc'
  , stripPrefix
  }

onlyNonEmpty :: OnlyNonEmpty NonEmptyList L.List
onlyNonEmpty =
  { makeCollection
  , makeCanEmptyCollection

  , fromFoldable
  , head
  , init
  , last
  , tail
  , uncons

  , fromList
  , toList
  }

onlyStrict :: OnlyStrict NonEmptyList
onlyStrict =
  { makeCollection

  , alterAt
  , insertAt
  , modifyAt
  , updateAt
  }