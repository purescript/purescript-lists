module Test.UpdatedTests(updatedTests) where

import Prelude

import Data.List as L
import Data.List.Lazy as LL
import Data.List.Lazy.NonEmpty as LNEL
import Data.List.NonEmpty as NEL
import Effect (Effect)
import Test.AllTests as T
import Test.Args.LazyList as LLA
import Test.Args.LazyNonEmptyList as LNELA
import Test.Args.List as LA
import Test.Args.NonEmptyList as NELA

{-
---  Next steps:

rebase
- fix "an list" -> "a list"
  - or even "a collection"
- upgrade to assertEqual


-}

updatedTests :: Effect Unit
updatedTests = do
  testBasicList
  testNonEmptyList
  testLazyList
  --testLazyNonEmptyList -- Lots of stuff to fix here

  -- Just using original ZipList tests
  {-
  Todo
  This is a wrapper on Lazy list. Should this be clarified in
  the name, and should there be a zip wrapper for non-lazy lists?
  Also, it doesn't seem like all instances are tested. Should
  testing be expanded?
  -}
  --testZipList

  -- Just using original ListPartial tests
  -- testListPartial

testBasicList :: Effect Unit
testBasicList = do

  T.printCollectionType "Basic List"

  T.testCommon LA.common
  T.testCommonDiffEmptiability T.RunAll LA.commonDiffEmptiability
  T.testOnlyCanEmpty LA.onlyCanEmpty
  T.testOnlyStrict LA.onlyStrict
  T.testOnlyStrictCanEmpty

testNonEmptyList :: Effect Unit
testNonEmptyList = do

  T.printCollectionType "NonEmpty List"

  T.testCommon NELA.common
  T.testCommonDiffEmptiability T.SkipBrokenStrictNonEmpty NELA.commonDiffEmptiability
  T.testOnlyNonEmpty NELA.onlyNonEmpty
  T.testOnlyStrict NELA.onlyStrict
  T.testOnlyStrictNonEmpty

testLazyList :: Effect Unit
testLazyList = do

  T.testCommon LLA.common
  T.testCommonDiffEmptiability T.SkipBrokenLazyCanEmpty LLA.commonDiffEmptiability
  T.testOnlyCanEmpty LLA.onlyCanEmpty
  T.testOnlyLazy LLA.onlyLazy
  T.testOnlyStrictCanEmpty
  T.testOnlyLazyCanEmpty


testLazyNonEmptyList :: Effect Unit
testLazyNonEmptyList = do

  T.printCollectionType "Lazy NonEmpty List"

  -- So much stuff is unsupported for this collection that it's not yet
  -- worth using the assertSkip strategy
  T.testCommon LNELA.common
  T.testCommonDiffEmptiability T.RunAll LNELA.commonDiffEmptiability
  T.testOnlyNonEmpty LNELA.onlyNonEmpty
  T.testOnlyLazy LNELA.onlyLazy
  T.testOnlyLazyNonEmpty

-- nil is passed instead of a singleton,
-- because some of the functions use this
-- as a convenience value
nil :: L.List Int
nil = L.Nil

lazyNil :: LL.List Int
lazyNil = LL.nil

nonEmpty :: NEL.NonEmptyList Int
nonEmpty = NEL.singleton 1

lazyNonEmpty :: LNEL.NonEmptyList Int
lazyNonEmpty = LNEL.singleton 1