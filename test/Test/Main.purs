module Test.Main where

import Prelude

import Effect (Effect)

import Test.UpdatedTests (updatedTests)

import Test.Data.List (testList)
import Test.Data.List.Lazy (testListLazy)
import Test.Data.List.NonEmpty (testNonEmptyList)
import Test.Data.List.Partial (testListPartial)
import Test.Data.List.ZipList (testZipList)

main :: Effect Unit
main = do
  --originalTests
  updatedTests
  pure unit

originalTests :: Effect Unit
originalTests = do
  testList
  testListLazy
  testZipList
  testListPartial
  testNonEmptyList