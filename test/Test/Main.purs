module Test.Main where

import Prelude
import Test.Data.List
import Test.Data.List.Lazy
import Test.Data.List.Unsafe

main = do
  testList
  testListLazy
  testListUnsafe
