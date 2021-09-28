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
  -- testNonEmptyList
  -- testLazyList
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
  T.testOnlyCanEmpty T.SkipBrokenStrictCanEmpty LA.onlyCanEmpty LA.common LA.commonDiffEmptiability
  T.testOnlyStrict LA.onlyStrict
  T.testOnlyStrictCanEmpty LA.onlyStrictCanEmpty

testNonEmptyList :: Effect Unit
testNonEmptyList = do

  T.printCollectionType "NonEmpty List"

  T.testCommon NELA.common
  T.testCommonDiffEmptiability T.SkipBrokenStrictNonEmpty NELA.commonDiffEmptiability
  T.testOnlyNonEmpty NELA.onlyNonEmpty NELA.commonDiffEmptiability
  T.testOnlyStrict NELA.onlyStrict
  T.testOnlyStrictNonEmpty NELA.onlyStrictNonEmpty

testLazyList :: Effect Unit
testLazyList = do

  T.testCommon LLA.common
  T.testCommonDiffEmptiability T.SkipBrokenLazyCanEmpty LLA.commonDiffEmptiability
  T.testOnlyCanEmpty T.SkipBrokenLazyCanEmpty LLA.onlyCanEmpty LLA.common LLA.commonDiffEmptiability
  T.testOnlyLazy LLA.onlyLazy LLA.common
  T.testOnlyLazyCanEmpty LLA.onlyLazyCanEmpty


testLazyNonEmptyList :: Effect Unit
testLazyNonEmptyList = do

  T.printCollectionType "Lazy NonEmpty List"

  -- So much stuff is unsupported for this collection that it's not yet
  -- worth using the assertSkip strategy
  T.testCommon LNELA.common
  T.testCommonDiffEmptiability T.RunAll LNELA.commonDiffEmptiability
  T.testOnlyNonEmpty LNELA.onlyNonEmpty LNELA.commonDiffEmptiability
  T.testOnlyLazy LNELA.onlyLazy LNELA.common
  T.testOnlyLazyNonEmpty LNELA.onlyLazyNonEmpty