module Tests.Data.List (runListTests) where

  import Debug.Trace

  import Test.QuickCheck
  import Test.QuickCheck.Gen

  import Data.List

  runListTests = do
    trace "Running List tests"
