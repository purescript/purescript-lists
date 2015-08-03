module Test.Data.List.Unsafe (testListUnsafe) where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.List (List(..), toList)
import Data.List.Unsafe
import Data.Maybe (Maybe(..))
import Test.Assert (assert, assertThrows)

testListUnsafe = do

  log "head should return a Just-wrapped first value of a non-empty list"
  assert $ head (toList ["foo", "bar"]) == "foo"

  log "head should throw an error for an empty list"
  assertThrows \_ -> head Nil

  log "last should return a Just-wrapped last value of a non-empty list"
  assert $ last (toList ["foo", "bar"]) == "bar"

  log "last should throw an error for an empty list"
  assertThrows \_ -> last Nil

  log "tail should return a Just-wrapped list containing all the items in an list apart from the first for a non-empty list"
  assert $ tail (toList ["foo", "bar", "baz"]) == toList ["bar", "baz"]

  log "tail should throw an error for an empty list"
  assertThrows \_ -> tail Nil

  log "init should return a Just-wrapped list containing all the items in an list apart from the first for a non-empty list"
  assert $ init (toList ["foo", "bar", "baz"]) == toList ["foo", "bar"]

  log "init should throw an error for an empty list"
  assertThrows \_ -> init Nil
