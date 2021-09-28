module Test.Args.LazyNonEmptyList where

import Data.List.Lazy.NonEmpty

import Data.Foldable (class Foldable)
import Data.List.Lazy as L
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude ((<<<))
import Test.API (Common, CommonDiffEmptiability, OnlyLazy, OnlyNonEmpty, OnlyLazyNonEmpty)

makeCollection :: forall a f. Foldable f => f a -> NonEmptyList a
makeCollection = unsafePartial fromJust <<< fromFoldable

makeCanEmptyCollection :: forall a f. Foldable f => f a -> L.List a
makeCanEmptyCollection = L.fromFoldable

makeNonEmptyCollection :: forall a f. Foldable f => f a -> NonEmptyList a
makeNonEmptyCollection = makeCollection

-- Suppress conversion to canEmpty list to enable common testing code
takeSimple :: forall a. Int -> NonEmptyList a -> NonEmptyList a
takeSimple n = unsafePartial fromJust <<< fromList <<< take n

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
  , makeInverseCollection: makeCanEmptyCollection

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

onlyLazy :: OnlyLazy NonEmptyList
onlyLazy =
  { makeCollection
  , takeSimple

  , insertAt
  , modifyAt
  , updateAt

  , iterate
  , repeat
  , cycle
  , foldrLazy
  , scanlLazy

  , replicate1
  , replicate1M
  }

onlyLazyNonEmpty :: OnlyLazyNonEmpty NonEmptyList L.List
onlyLazyNonEmpty =
  { alterAt
  , deleteAt
  }