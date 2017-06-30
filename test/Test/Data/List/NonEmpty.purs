module Test.Data.List.NonEmpty (testNonEmptyList) where

import Prelude
import Data.List (fromFoldable, range)
import Data.List.NonEmpty as NEL
import Data.NonEmpty ((:|))
import Data.Maybe
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT, assert)

testNonEmptyList ::
  forall eff. Eff (assert :: ASSERT, console :: CONSOLE | eff) Unit
testNonEmptyList = do
  let nel x xs = NEL.NonEmptyList $ x :| fromFoldable xs

  log "sort should reorder a non-empty list into ascending order based on the result of compare"
  assert $ NEL.sort (nel 1 [3, 2, 5, 6, 4]) == nel 1 [2, 3, 4, 5, 6]

  log "sortBy should reorder a non-empty list into ascending order based on the result of a comparison function"
  assert $ NEL.sortBy (flip compare) (nel 1 [3, 2, 5, 6, 4]) == nel 6 [5, 4, 3, 2, 1]

  log "traverse1 should be stack-safe"
  let xs = NEL.NonEmptyList $ 0 :| range 1 100000
  assert $ NEL.traverse1 Just xs == Just xs

  log "traverse1 should preserve order"
  let xs = nel 0 [1, 2, 3, 4, 5]
  assert $ NEL.traverse1 Just xs == Just xs
