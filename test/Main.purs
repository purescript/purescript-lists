module Test.Main where

import Prelude

import Debug.Trace

import Prelude hiding ((:))

import Data.List
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))

import qualified Data.List.Lazy as L

import Control.Monad.Eff

foreign import data ASSERT :: !

foreign import assert
  "function assert(success) {\
  \  return function () {\
  \    if (success) return {};\
  \    throw new Error('Assertion failed');\
  \  };\
  \}" :: forall e. Boolean -> Eff (assert :: ASSERT | e) Unit

main = do
  trace "test equality"
  assert $ toList [1] == toList [1]
  assert $ toList [1, 2, 3] == toList [1, 2, 3]

  trace "test inequality"
  assert $ toList [1] /= toList [2]
  assert $ toList [1, 2, 3] /= toList [1, 2, 2]

  trace "(!!) should return Just x when the index is within the bounds of the array"
  assert $ (toList [1, 2, 3] !! 0) == (Just 1)
  assert $ (toList [1, 2, 3] !! 1) == (Just 2)
  assert $ (toList [1, 2, 3] !! 2) == (Just 3)

  trace "(!!) should return Nothing when the index is outside of the bounds of the array"
  assert $ (toList [1, 2, 3] !! 6) == Nothing
  assert $ (toList [1, 2, 3] !! (-1)) == Nothing

  trace "singleton should construct an array with a single value"
  assert $ singleton 1 == toList [1]
  assert $ singleton "foo" == toList ["foo"]
  assert $ singleton nil == toList [nil]

  trace "head should return a Just-wrapped first value of a non-empty array"
  assert $ head (toList ["foo", "bar"]) == Just "foo"

  trace "head should return Nothing for an empty array"
  assert $ head nil == Nothing

  trace "last should return a Just-wrapped last value of a non-empty array"
  assert $ last (toList ["foo", "bar"]) == Just "bar"

  trace "last should return Nothing for an empty array"
  assert $ last nil == Nothing

  trace "tail should return a Just-wrapped array containing all the items in an array apart from the first for a non-empty array"
  assert $ tail (toList ["foo", "bar", "baz"]) == Just (toList ["bar", "baz"])

  trace "tail should return Nothing for an empty array"
  assert $ tail nil == Nothing

  trace "init should return a Just-wrapped array containing all the items in an array apart from the first for a non-empty array"
  assert $ init (toList ["foo", "bar", "baz"]) == Just (toList ["foo", "bar"])

  trace "init should return Nothing for an empty array"
  assert $ init nil == Nothing

  trace "null should return false for non-empty arrays"
  assert $ null (toList [1]) == false
  assert $ null (toList [1, 2, 3]) == false

  trace "null should return true for an empty array"
  assert $ null nil == true

  trace "length should return the number of items in an array"
  assert $ length nil == 0
  assert $ length (singleton 1) == 1
  assert $ length (toList [1, 2, 3, 4, 5]) == 5

  trace "append should joint two arrays"
  assert $ (toList [1, 2] <> toList [3, 4]) == toList [1, 2, 3, 4]
  assert $ (toList [1] <> nil) == toList [1]
  assert $ (nil <> nil) == nil

  trace "concat should join an array of arrays"
  assert $ concat (toList [toList [1, 2], toList [3, 4]]) == toList [1, 2, 3, 4]
  assert $ concat (toList [singleton 1, nil]) == singleton 1
  assert $ concat (toList [nil, nil]) == nil

  trace "reverse should reverse the order of items in an array"
  assert $ (reverse (toList [1, 2, 3])) == toList [3, 2, 1]
  assert $ (reverse nil) == nil

  trace "span should split an array in two based on a predicate"
  case span (< 4) (toList [1, 2, 3, 4, 5, 6, 7]) of
    Tuple init rest -> do
      assert $ init == toList [1, 2, 3]
      assert $ rest == toList [4, 5, 6, 7]

  trace "drop should remove the specified number of items from the front of an array"
  assert $ (drop 1 (toList [1, 2, 3])) == toList [2, 3]
  assert $ (drop 2 (toList [1, 2, 3])) == toList [3]
  assert $ (drop 1 nil) == nil

  trace "dropWhile should remove all values that match a predicate from the front of an array"
  assert $ (dropWhile (/= 1) (toList [1, 2, 3])) == toList [1, 2, 3]
  assert $ (dropWhile (/= 2) (toList [1, 2, 3])) == toList [2, 3]
  assert $ (dropWhile (/= 1) nil) == nil

  trace "take should keep the specified number of items from the front of an array, discarding the rest"
  assert $ (take 1 (toList [1, 2, 3])) == toList [1]
  assert $ (take 2 (toList [1, 2, 3])) == toList [1, 2]
  assert $ (take 1 nil) == nil

  trace "takeWhile should keep all values that match a predicate from the front of an array"
  assert $ (takeWhile (/= 2) (toList [1, 2, 3])) == toList [1]
  assert $ (takeWhile (/= 3) (toList [1, 2, 3])) == toList [1, 2]
  assert $ (takeWhile (/= 1) nil) == nil

  trace "insertAt should add an item at the specified index"
  assert $ (insertAt 0 1 (toList [2, 3])) == Just (toList [1, 2, 3])
  assert $ (insertAt 1 1 (toList [2, 3])) == Just (toList [2, 1, 3])
  assert $ (insertAt 2 1 (toList [2, 3])) == Just (toList [2, 3, 1])
  assert $ (insertAt 3 1 (toList [2, 3])) == Nothing

  trace "deleteAt should remove an item at the specified index"
  assert $ (deleteAt 0 (toList [1, 2, 3])) == Just (toList [2, 3])
  assert $ (deleteAt 1 (toList [1, 2, 3])) == Just (toList [1, 3])
  assert $ (deleteAt 2 (toList [1, 2, 3])) == Just (toList [1, 2])
  assert $ (deleteAt 3 (toList [1, 2, 3])) == Nothing

  trace "updateAt should replace an item at the specified index"
  assert $ (updateAt 0 9 (toList [1, 2, 3])) == Just (toList [9, 2, 3])
  assert $ (updateAt 1 9 (toList [1, 2, 3])) == Just (toList [1, 9, 3])
  assert $ (updateAt 2 9 (toList [1, 2, 3])) == Just (toList [1, 2, 9])
  assert $ (updateAt 1 9 nil) == Nothing

  trace "modifyAt should update an item at the specified index"
  assert $ (modifyAt 0 (+ 1) (toList [1, 2, 3])) == Just (toList [2, 2, 3])
  assert $ (modifyAt 1 (+ 1) (toList [1, 2, 3])) == Just (toList [1, 3, 3])
  assert $ (modifyAt 2 (+ 1) (toList [1, 2, 3])) == Just (toList [1, 2, 4])
  assert $ (modifyAt 1 (+ 1) nil) == Nothing

  trace "delete should remove the first matching item from an array"
  assert $ delete 1 (toList [1, 2, 1]) == toList [2, 1]
  assert $ delete 2 (toList [1, 2, 1]) == toList [1, 1]

  trace "deleteBy should remove the first equality-relation-matching item from an array"
  assert $ deleteBy (/=) 2 (toList [1, 2, 1]) == toList [2, 1]
  assert $ deleteBy (/=) 1 (toList [1, 2, 1]) == toList [1, 1]

  trace "(\\\\) should return the difference between two lists"
  assert $ toList [1, 2, 3, 4, 3, 2, 1] \\ toList [1, 1, 2, 3] == toList [4, 3, 2]

  trace "intersect should return the intersection of two arrays"
  assert $ intersect (toList [1, 2, 3, 4, 3, 2, 1]) (toList [1, 1, 2, 3]) == toList [1, 2, 3, 3, 2, 1]

  trace "intersect should return the intersection of two arrays using the specified equivalence relation"
  assert $ intersectBy (\x y -> (x * 2) == y) (toList [1, 2, 3]) (toList [2, 6]) == toList [1, 3]

  trace "concatMap should be equivalent to (concat <<< map)"
  assert $ concatMap doubleAndOrig (toList [1, 2, 3]) == concat (doubleAndOrig <$> toList [1, 2, 3])

  trace "map should transform every item in an array"
  assert $ ((* 2) <$> toList [1, 2, 3]) == toList [2, 4, 6]

  trace "mapMaybe should transform every item in an array, throwing out Nothing values"
  assert $ mapMaybe (\x -> if x /= 0 then Just x else Nothing) (toList [0, 1, 0, 0, 2, 3]) == toList[1, 2, 3]

  trace "catMaybe should take an array of Maybe values and throw out Nothings"
  assert $ catMaybes (toList [Nothing, Just 2, Nothing, Just 4]) == toList [2, 4]

  trace "filter should remove items that don't match a predicate"
  assert $ filter odd (toList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) == toList [1, 3, 5, 7, 9]

  trace "zipWith should use the specified function to zip two lists together"
  assert $ zipWith (\x y -> [show x, y]) (toList [1, 2, 3]) (toList ["a", "b", "c"]) == 
      toList [["1", "a"], ["2", "b"], ["3", "c"]]

  trace "nub should remove duplicate items from the list"
  assert $ nub (toList [1, 2, 2, 3, 4, 1]) == toList [1, 2, 3, 4]

  trace "nubBy should remove duplicate items from the list using a supplied predicate"
  let nubPred = \x y -> if odd x then false else x == y
  assert $ nubBy nubPred (toList [1, 2, 2, 3, 3, 4, 4, 1]) == toList [1, 2, 3, 3, 4, 1]
  
  trace "can find the first 10 primes using lazy lists"  
  let eratos :: L.List Number -> L.List Number
      eratos xs = Control.Lazy.defer \_ -> 
        case L.uncons xs of
          Nothing -> L.nil
          Just (Tuple p xs) -> p `L.cons` eratos (L.filter (\x -> x % p /= 0) xs)
      
      upFrom = L.iterate (1 +)

      primes = eratos $ upFrom 2
  assert $ L.fromList (L.take 10 primes) == [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

nil :: List Number
nil = Nil

odd :: Number -> Boolean
odd n = n % 2 /= 0

doubleAndOrig :: Number -> List Number
doubleAndOrig x = Cons (x * 2) (Cons x Nil)
