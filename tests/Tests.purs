module Main where

import Debug.Trace

import Test.QuickCheck
import Test.QuickCheck.LCG

import Tests.Data.List
import Tests.Data.ListT

main = do
  runListTests
  runListTTests
