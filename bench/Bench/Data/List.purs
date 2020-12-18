module Bench.Data.List where

import Prelude
import Data.Maybe (fromMaybe)
import Data.Foldable (maximum)
import Data.List (List(..), take, range, foldr, length, (:))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Console (log)
import Performance.Minibench (bench)

benchList :: Effect Unit
benchList = do
  benchLists "map" $ map (_ + 1)
  benchLists "foldr" $ foldr add 0

  where

  listSizes = 0 : 1 : 1000 : 2000 : 5000 : 10000 : 100000 : Nil
  nats = range 0 $ (fromMaybe 0 $ maximum listSizes) - 1
  lists = map (\n -> take n nats) listSizes

  benchLists :: forall b. String -> (List Int -> b) -> Effect Unit
  benchLists label func =
    traverse_ (benchAList label func) lists

  benchAList :: forall a b. String -> (List a -> b) -> List a -> Effect Unit
  benchAList label func list = do
    log "---"
    log $ label <> ": list (" <> show (length list) <> " elems)"
    bench \_ -> func list