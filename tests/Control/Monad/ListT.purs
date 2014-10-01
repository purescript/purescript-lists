module Tests.Data.ListT (runListTTests) where

  import Debug.Trace

  import Control.Monad.Identity

  import Test.QuickCheck
  import Test.QuickCheck.LCG
  import Data.Tuple
  import Data.Maybe

  import Control.Monad.ListT
  import qualified Data.Array as A

  data ZeroToTen = ZeroToTen Number

  runZeroToTen :: ZeroToTen -> Number
  runZeroToTen (ZeroToTen n) = n

  instance arbZeroToTen :: Arbitrary ZeroToTen where
    arbitrary = ZeroToTen <$> chooseInt 0 10

  checkFromToArray a = 
    (runIdentity $ (toArray <<< fromArray) a) == (a :: [Number]) <?> "toArray <<< fromArray == id"

  checkTake (Tuple a (ZeroToTen n)) = 
    (runIdentity $ (toArray <<< (take n) <<< fromArray) a) == A.take n (a :: [Number]) <?> "take"

  checkIterate (ZeroToTen n) =
    (runIdentity $ head $ iterate ((+) 1) n) == Just n <?> "iterate"

  runListTTests = do
    trace "Running ListT tests"

    quickCheck $ checkFromToArray

    quickCheck $ checkTake

    quickCheck $ checkIterate