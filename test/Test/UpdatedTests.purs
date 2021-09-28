module Test.UpdatedTests(updatedTests) where

import Prelude

import Data.List as L
import Data.List.Lazy as LL
import Data.List.Lazy.NonEmpty as LNEL
import Data.List.NonEmpty as NEL
import Effect (Effect)
import Test.Common (testCommon, SkipBroken(..), printCollectionType)
import Test.CommonDiffEmptiability (testCommonDiffEmptiability)
import Test.NoOverlap (testOnlyLazyCanEmpty, testOnlyLazyNonEmpty, testOnlyStrictCanEmpty, testOnlyStrictNonEmpty)
import Test.OnlyCanEmpty (testOnlyCanEmpty)
import Test.OnlyLazy (testOnlyLazy)
import Test.OnlyNonEmpty (testOnlyNonEmpty)
import Test.OnlyStrict (testOnlyStrict)


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

  printCollectionType "Basic List"

  testCommon nil
  testCommonDiffEmptiability RunAll nil nil nonEmpty
  testOnlyCanEmpty nil nonEmpty
  testOnlyStrict nil
  testOnlyStrictCanEmpty

testNonEmptyList :: Effect Unit
testNonEmptyList = do

  printCollectionType "NonEmpty List"

  testCommon nonEmpty
  testCommonDiffEmptiability RunAll nonEmpty nil nonEmpty
  testOnlyNonEmpty nonEmpty nil
  testOnlyStrict nonEmpty
  testOnlyStrictNonEmpty

testLazyList :: Effect Unit
testLazyList = do

  printCollectionType "Lazy List"

  testCommon lazyNil
  testCommonDiffEmptiability SkipBrokenLazyCanEmpty lazyNil lazyNil lazyNonEmpty
  testOnlyCanEmpty lazyNil lazyNonEmpty
  testOnlyLazy lazyNil
  testOnlyLazyCanEmpty

testLazyNonEmptyList :: Effect Unit
testLazyNonEmptyList = do

  printCollectionType "Lazy NonEmpty List"

  -- So much stuff is unsupported for this collection that it's not yet
  -- worth using the assertSkip strategy
  testCommon lazyNonEmpty
  testCommonDiffEmptiability RunAll lazyNonEmpty lazyNil lazyNonEmpty
  testOnlyNonEmpty lazyNonEmpty lazyNil
  testOnlyLazy lazyNonEmpty
  testOnlyLazyNonEmpty

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