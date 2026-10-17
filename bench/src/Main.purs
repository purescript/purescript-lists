module Bench.Main where

import Prelude

import Bench.Data.List (benchList)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "List"
  log "===="
  benchList
