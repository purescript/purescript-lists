module Test.Data.List.NonEmpty (testNonEmptyList) where

import Prelude
import Data.List (fromFoldable)
import Data.List.NonEmpty (NonEmptyList(..), sort, sortBy)
import Data.NonEmpty ((:|))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT, assert)

testNonEmptyList ::
  forall eff. Eff (assert :: ASSERT, console :: CONSOLE | eff) Unit
testNonEmptyList = do
  let nel x xs = NonEmptyList $ x :| fromFoldable xs

  log "sort should reorder a non-empty list into ascending order based on the result of compare"
  assert $ sort (nel 1 [3, 2, 5, 6, 4]) == nel 1 [2, 3, 4, 5, 6]
  log "sortBy should reorder a non-empty list into ascending order based on the result of a comparison function"
  assert $ sortBy (flip compare) (nel 1 [3, 2, 5, 6, 4]) == nel 6 [5, 4, 3, 2, 1]
