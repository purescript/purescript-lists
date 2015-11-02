module Test.Main where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE())
import Test.Assert (ASSERT())
import Test.Data.List
import Test.Data.List.Lazy
import Test.Data.List.Unsafe

main :: forall eff. Eff (assert :: ASSERT, console :: CONSOLE | eff) Unit
main = do
  testList
  testListLazy
  testListUnsafe
