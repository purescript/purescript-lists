module Tests.Data.List where

  import Debug.Trace

  import Test.QuickCheck
  import Test.QuickCheck.LCG

  import Data.List

  runListTests = do
    trace "Running List tests"
