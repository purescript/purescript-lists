module Test.Main where

import Prelude (Unit, bind)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE())
import Test.Assert (ASSERT())
import Test.Data.List (testList)
import Test.Data.List.Lazy (testListLazy)
import Test.Data.List.ZipList (testZipList)
import Test.Data.List.Unsafe (testListUnsafe)

main :: forall eff. Eff (assert :: ASSERT, console :: CONSOLE | eff) Unit
main = do
  testList
  testListLazy
  testZipList
  testListUnsafe
