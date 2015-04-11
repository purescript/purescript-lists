module Test.Control.Monad.ListT (checkListT) where

import Control.Monad.ListT
import Data.Identity (runIdentity)
import Data.Int (Int(), fromNumber)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Debug.Trace (trace)
import Test.QuickCheck ((<?>), quickCheck, Result())
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (chooseInt)
import qualified Data.Array as A

instance arbitraryListT :: (Monad f, Arbitrary a) => Arbitrary (ListT f a) where
  arbitrary = fromArray <$> arbitrary

data ZeroToTen = ZeroToTen Int

runZeroToTen :: ZeroToTen -> Int
runZeroToTen (ZeroToTen n) = n

instance arbZeroToTen :: Arbitrary ZeroToTen where
  arbitrary = ZeroToTen <$> chooseInt zero (fromNumber 10)

checkFromToArray :: [Int] -> Result
checkFromToArray a =
  runIdentity ((toArray <<< fromArray) a) == a <?> "toArray <<< fromArray == id"

checkTake :: Tuple [Int] ZeroToTen -> Result
checkTake (Tuple a (ZeroToTen n)) =
  runIdentity (toArray $ take n $ fromArray a) == A.take n a <?> "take"

checkIterate :: ZeroToTen -> Result
checkIterate (ZeroToTen n) =
  runIdentity (head $ iterate (+ one) n) == Just n <?> "iterate"

checkListT = do
  trace "Checking ListT"
  quickCheck checkFromToArray
  quickCheck checkTake
  quickCheck checkIterate
