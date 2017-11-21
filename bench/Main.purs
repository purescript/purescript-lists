module Bench.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Bench.Data.List (benchList)

main :: Eff (console :: CONSOLE) Unit
main = do
  log "List"
  log "===="
  benchList
