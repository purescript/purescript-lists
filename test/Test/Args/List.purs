module Test.Args.List where

import Data.List

import Data.Foldable (class Foldable)
import Data.List.NonEmpty as NEL
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude ((<<<))
import Test.API (Common, CommonDiffEmptiability, OnlyCanEmpty, OnlyStrict, OnlyStrictCanEmpty)

makeCollection :: forall a f. Foldable f => f a -> List a
makeCollection = fromFoldable

makeCanEmptyCollection :: forall a f. Foldable f => f a -> List a
makeCanEmptyCollection = fromFoldable

makeNonEmptyCollection :: forall a f. Foldable f => f a -> NEL.NonEmptyList a
makeNonEmptyCollection = unsafePartial fromJust <<< NEL.fromFoldable

common :: Common List
common =
  { makeCollection

  , concat
  , concatMap
  , cons: Cons
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

commonDiffEmptiability :: CommonDiffEmptiability List NEL.NonEmptyList List NEL.NonEmptyList Pattern
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

onlyCanEmpty :: OnlyCanEmpty List NEL.NonEmptyList
onlyCanEmpty =
  { makeCollection
  , makeNonEmptyCollection

  , fromFoldable
  , head
  , init
  , last
  , tail
  , uncons

  , null
  , many
  , manyRec
  }

onlyStrict :: OnlyStrict List
onlyStrict =
  { makeCollection

  , alterAt
  , insertAt
  , modifyAt
  , updateAt
  }

onlyStrictCanEmpty :: OnlyStrictCanEmpty List
onlyStrictCanEmpty =
  { deleteAt }