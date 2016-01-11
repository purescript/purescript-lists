module Test.Data.List.Unsafe (testListUnsafe) where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.List (List(..), fromFoldable)
import Data.List.Unsafe
import Test.Assert (ASSERT(), assert, assertThrows)

testListUnsafe :: forall eff. Eff (assert :: ASSERT, console :: CONSOLE | eff) Unit
testListUnsafe = do
  let l = fromFoldable

  log "head should return a Just-wrapped first value of a non-empty list"
  assert $ head (l ["foo", "bar"]) == "foo"

  log "head should throw an error for an empty list"
  assertThrows \_ -> head Nil

  log "last should return a Just-wrapped last value of a non-empty list"
  assert $ last (l ["foo", "bar"]) == "bar"

  log "last should throw an error for an empty list"
  assertThrows \_ -> last Nil

  log "tail should return a Just-wrapped list containing all the items in an list apart from the first for a non-empty list"
  assert $ tail (l ["foo", "bar", "baz"]) == l ["bar", "baz"]

  log "tail should throw an error for an empty list"
  assertThrows \_ -> tail Nil

  log "init should return a Just-wrapped list containing all the items in an list apart from the first for a non-empty list"
  assert $ init (l ["foo", "bar", "baz"]) == l ["foo", "bar"]

  log "init should throw an error for an empty list"
  assertThrows \_ -> init Nil
