module Test.Data.List (testList) where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.Foldable
import Data.Monoid.Additive
import Data.List
import Data.Maybe (Maybe(..), isNothing)
import Data.Maybe.Unsafe (fromJust)
import Data.Tuple (Tuple(..))
import Test.Assert (ASSERT(), assert)

testList :: forall eff. Eff (assert :: ASSERT, console :: CONSOLE | eff) Unit
testList = do

  log "singleton should construct an list with a single value"
  assert $ singleton 1 == toList [1]
  assert $ singleton "foo" == toList ["foo"]
  assert $ singleton nil == toList [toList []]

  log "range should create an inclusive list of integers for the specified start and end"
  assert $ (range 0 5) == toList [0, 1, 2, 3, 4, 5]
  assert $ (range 2 (-3)) == toList [2, 1, 0, -1, -2, -3]

  log "replicate should produce an list containg an item a specified number of times"
  assert $ replicate 3 true == toList [true, true, true]
  assert $ replicate 1 "foo" == toList ["foo"]
  assert $ replicate 0 "foo" == toList []
  assert $ replicate (-1) "foo" == toList []

  log "replicateM should perform the monadic action the correct number of times"
  assert $ replicateM 3 (Just 1) == Just (toList [1, 1, 1])
  assert $ replicateM 1 (Just 1) == Just (toList [1])
  assert $ replicateM 0 (Just 1) == Just (toList [])
  assert $ replicateM (-1) (Just 1) == Just (toList [])

  -- some
  -- many

  log "null should return false for non-empty lists"
  assert $ null (toList [1]) == false
  assert $ null (toList [1, 2, 3]) == false

  log "null should return true for an empty list"
  assert $ null nil == true

  log "length should return the number of items in an list"
  assert $ length nil == 0
  assert $ length (toList [1]) == 1
  assert $ length (toList [1, 2, 3, 4, 5]) == 5

  log "length should be stack-safe"
  void $ pure $ length (range 1 100000)

  log "snoc should add an item to the end of an list"
  assert $ toList [1, 2, 3] `snoc` 4 == toList [1, 2, 3, 4]
  assert $ nil `snoc` 1 == toList [1]

  log "insert should add an item at the appropriate place in a sorted list"
  assert $ insert 1.5 (toList [1.0, 2.0, 3.0]) == toList [1.0, 1.5, 2.0, 3.0]
  assert $ insert 4 (toList [1, 2, 3]) == toList [1, 2, 3, 4]
  assert $ insert 0 (toList [1, 2, 3]) == toList [0, 1, 2, 3]

  log "insertBy should add an item at the appropriate place in a sorted list using the specified comparison"
  assert $ insertBy (flip compare) 4 (toList [1, 2, 3]) == toList [4, 1, 2, 3]
  assert $ insertBy (flip compare) 0 (toList [1, 2, 3]) == toList [1, 2, 3, 0]

  log "head should return a Just-wrapped first value of a non-empty list"
  assert $ head (toList ["foo", "bar"]) == Just "foo"

  log "head should return Nothing for an empty list"
  assert $ head nil == Nothing

  log "last should return a Just-wrapped last value of a non-empty list"
  assert $ last (toList ["foo", "bar"]) == Just "bar"

  log "last should return Nothing for an empty list"
  assert $ last nil == Nothing

  log "tail should return a Just-wrapped list containing all the items in an list apart from the first for a non-empty list"
  assert $ tail (toList ["foo", "bar", "baz"]) == Just (toList ["bar", "baz"])

  log "tail should return Nothing for an empty list"
  assert $ tail nil == Nothing

  log "init should return a Just-wrapped list containing all the items in an list apart from the first for a non-empty list"
  assert $ init (toList ["foo", "bar", "baz"]) == Just (toList ["foo", "bar"])

  log "init should return Nothing for an empty list"
  assert $ init nil == Nothing

  log "uncons should return nothing when used on an empty list"
  assert $ isNothing (uncons nil)

  log "uncons should split an list into a head and tail record when there is at least one item"
  let u1 = uncons (toList [1])
  assert $ (fromJust u1).head == 1
  assert $ (fromJust u1).tail == toList []
  let u2 = uncons (toList [1, 2, 3])
  assert $ (fromJust u2).head == 1
  assert $ (fromJust u2).tail == toList [2, 3]

  log "(!!) should return Just x when the index is within the bounds of the list"
  assert $ toList [1, 2, 3] !! 0 == (Just 1)
  assert $ toList [1, 2, 3] !! 1 == (Just 2)
  assert $ toList [1, 2, 3] !! 2 == (Just 3)

  log "(!!) should return Nothing when the index is outside of the bounds of the list"
  assert $ toList [1, 2, 3] !! 6 == Nothing
  assert $ toList [1, 2, 3] !! (-1) == Nothing

  log "elemIndex should return the index of an item that a predicate returns true for in an list"
  assert $ elemIndex 1 (toList [1, 2, 1]) == Just 0
  assert $ elemIndex 4 (toList [1, 2, 1]) == Nothing

  log "elemLastIndex should return the last index of an item in an list"
  assert $ elemLastIndex 1 (toList [1, 2, 1]) == Just 2
  assert $ elemLastIndex 4 (toList [1, 2, 1]) == Nothing

  log "findIndex should return the index of an item that a predicate returns true for in an list"
  assert $ findIndex (/= 1) (toList [1, 2, 1]) == Just 1
  assert $ findIndex (== 3) (toList [1, 2, 1]) == Nothing

  log "findLastIndex should return the last index of an item in an list"
  assert $ findLastIndex (/= 1) (toList [2, 1, 2]) == Just 2
  assert $ findLastIndex (== 3) (toList [2, 1, 2]) == Nothing

  log "insertAt should add an item at the specified index"
  assert $ (insertAt 0 1 (toList [2, 3])) == Just (toList [1, 2, 3])
  assert $ (insertAt 1 1 (toList [2, 3])) == Just (toList [2, 1, 3])
  assert $ (insertAt 2 1 (toList [2, 3])) == Just (toList [2, 3, 1])

  log "insertAt should return Nothing if the index is out of range"
  assert $ (insertAt 2 1 nil) == Nothing

  log "deleteAt should remove an item at the specified index"
  assert $ (deleteAt 0 (toList [1, 2, 3])) == Just (toList [2, 3])
  assert $ (deleteAt 1 (toList [1, 2, 3])) == Just (toList [1, 3])

  log "deleteAt should return Nothing if the index is out of range"
  assert $ (deleteAt 1 nil) == Nothing

  log "updateAt should replace an item at the specified index"
  assert $ (updateAt 0 9 (toList [1, 2, 3])) == Just (toList [9, 2, 3])
  assert $ (updateAt 1 9 (toList [1, 2, 3])) == Just (toList [1, 9, 3])

  log "updateAt should return Nothing if the index is out of range"
  assert $ (updateAt 1 9 nil) == Nothing

  log "modifyAt should update an item at the specified index"
  assert $ (modifyAt 0 (+ 1) (toList [1, 2, 3])) == Just (toList [2, 2, 3])
  assert $ (modifyAt 1 (+ 1) (toList [1, 2, 3])) == Just (toList [1, 3, 3])

  log "modifyAt should return Nothing if the index is out of range"
  assert $ (modifyAt 1 (+ 1) nil) == Nothing

  log "alterAt should update an item at the specified index when the function returns Just"
  assert $ (alterAt 0 (Just <<< (+ 1)) (toList [1, 2, 3])) == Just (toList [2, 2, 3])
  assert $ (alterAt 1 (Just <<< (+ 1)) (toList [1, 2, 3])) == Just (toList [1, 3, 3])

  log "alterAt should drop an item at the specified index when the function returns Nothing"
  assert $ (alterAt 0 (const Nothing) (toList [1, 2, 3])) == Just (toList [2, 3])
  assert $ (alterAt 1 (const Nothing) (toList [1, 2, 3])) == Just (toList [1, 3])

  log "alterAt should return Nothing if the index is out of range"
  assert $ (alterAt 1 (Just <<< (+ 1)) nil) == Nothing

  log "reverse should reverse the order of items in an list"
  assert $ (reverse (toList [1, 2, 3])) == toList [3, 2, 1]
  assert $ (reverse nil) == nil

  log "concat should join an list of lists"
  assert $ (concat (toList [toList [1, 2], toList [3, 4]])) == toList [1, 2, 3, 4]
  assert $ (concat (toList [toList [1], nil])) == toList [1]
  assert $ (concat (toList [nil, nil])) == nil

  log "concatMap should be equivalent to (concat <<< map)"
  assert $ concatMap doubleAndOrig (toList [1, 2, 3]) == concat (map doubleAndOrig (toList [1, 2, 3]))

  log "filter should remove items that don't match a predicate"
  assert $ filter odd (range 0 10) == toList [1, 3, 5, 7, 9]

  log "filterM should remove items that don't match a predicate while using a monadic behaviour"
  assert $ filterM (Just <<< odd) (range 0 10) == Just (toList [1, 3, 5, 7, 9])
  assert $ filterM (const Nothing) (range 0 10) == Nothing

  log "mapMaybe should transform every item in an list, throwing out Nothing values"
  assert $ mapMaybe (\x -> if x /= 0 then Just x else Nothing) (toList [0, 1, 0, 0, 2, 3]) == toList [1, 2, 3]

  log "catMaybe should take an list of Maybe values and throw out Nothings"
  assert $ catMaybes (toList [Nothing, Just 2, Nothing, Just 4]) == toList [2, 4]

  log "sort should reorder a list into ascending order based on the result of compare"
  assert $ sort (toList [1, 3, 2, 5, 6, 4]) == toList [1, 2, 3, 4, 5, 6]

  log "sortBy should reorder a list into ascending order based on the result of a comparison function"
  assert $ sortBy (flip compare) (toList [1, 3, 2, 5, 6, 4]) == toList [6, 5, 4, 3, 2, 1]

  log "take should keep the specified number of items from the front of an list, discarding the rest"
  assert $ (take 1 (toList [1, 2, 3])) == toList [1]
  assert $ (take 2 (toList [1, 2, 3])) == toList [1, 2]
  assert $ (take 1 nil) == nil

  log "takeWhile should keep all values that match a predicate from the front of an list"
  assert $ (takeWhile (/= 2) (toList [1, 2, 3])) == toList [1]
  assert $ (takeWhile (/= 3) (toList [1, 2, 3])) == toList [1, 2]
  assert $ (takeWhile (/= 1) nil) == nil

  log "drop should remove the specified number of items from the front of an list"
  assert $ (drop 1 (toList [1, 2, 3])) == toList [2, 3]
  assert $ (drop 2 (toList [1, 2, 3])) == toList [3]
  assert $ (drop 1 nil) == nil

  log "dropWhile should remove all values that match a predicate from the front of an list"
  assert $ (dropWhile (/= 1) (toList [1, 2, 3])) == toList [1, 2, 3]
  assert $ (dropWhile (/= 2) (toList [1, 2, 3])) == toList [2, 3]
  assert $ (dropWhile (/= 1) nil) == nil

  log "span should split an list in two based on a predicate"
  let spanResult = span (< 4) (toList [1, 2, 3, 4, 5, 6, 7])
  assert $ spanResult.init == toList [1, 2, 3]
  assert $ spanResult.rest == toList [4, 5, 6, 7]

  log "group should group consecutive equal elements into lists"
  assert $ group (toList [1, 2, 2, 3, 3, 3, 1]) == toList [toList [1], toList [2, 2], toList [3, 3, 3], toList [1]]

  log "group' should sort then group consecutive equal elements into lists"
  assert $ group' (toList [1, 2, 2, 3, 3, 3, 1]) == toList [toList [1, 1], toList [2, 2], toList [3, 3, 3]]

  log "groupBy should group consecutive equal elements into lists based on an equivalence relation"
  assert $ groupBy (\x y -> odd x && odd y) (toList [1, 1, 2, 2, 3, 3]) == toList [toList [1, 1], toList [2], toList [2], toList [3, 3]]

  log "nub should remove duplicate elements from the list, keeping the first occurence"
  assert $ nub (toList [1, 2, 2, 3, 4, 1]) == toList [1, 2, 3, 4]

  log "nubBy should remove duplicate items from the list using a supplied predicate"
  let nubPred = \x y -> if odd x then false else x == y
  assert $ nubBy nubPred (toList [1, 2, 2, 3, 3, 4, 4, 1]) == toList [1, 2, 3, 3, 4, 1]

  log "union should produce the union of two lists"
  assert $ union (toList [1, 2, 3]) (toList [2, 3, 4]) == toList [1, 2, 3, 4]
  assert $ union (toList [1, 1, 2, 3]) (toList [2, 3, 4]) == toList [1, 1, 2, 3, 4]

  log "unionBy should produce the union of two lists using the specified equality relation"
  assert $ unionBy (\_ y -> y < 5) (toList [1, 2, 3]) (toList [2, 3, 4, 5, 6]) == toList [1, 2, 3, 5, 6]

  log "delete should remove the first matching item from an list"
  assert $ delete 1 (toList [1, 2, 1]) == toList [2, 1]
  assert $ delete 2 (toList [1, 2, 1]) == toList [1, 1]

  log "deleteBy should remove the first equality-relation-matching item from an list"
  assert $ deleteBy (/=) 2 (toList [1, 2, 1]) == toList [2, 1]
  assert $ deleteBy (/=) 1 (toList [1, 2, 1]) == toList [1, 1]

  log "(\\\\) should return the difference between two lists"
  assert $ toList [1, 2, 3, 4, 3, 2, 1] \\ toList [1, 1, 2, 3] == toList [4, 3, 2]

  log "intersect should return the intersection of two lists"
  assert $ intersect (toList [1, 2, 3, 4, 3, 2, 1]) (toList [1, 1, 2, 3]) == toList [1, 2, 3, 3, 2, 1]

  log "intersectBy should return the intersection of two lists using the specified equivalence relation"
  assert $ intersectBy (\x y -> (x * 2) == y) (toList [1, 2, 3]) (toList [2, 6]) == toList [1, 3]

  log "zipWith should use the specified function to zip two lists together"
  assert $ zipWith (\x y -> toList [show x, y]) (toList [1, 2, 3]) (toList ["a", "b", "c"]) == toList [toList ["1", "a"], toList ["2", "b"], toList ["3", "c"]]

  log "zipWithA should use the specified function to zip two lists together"
  assert $ zipWithA (\x y -> Just $ Tuple x y) (toList [1, 2, 3]) (toList ["a", "b", "c"]) == Just (toList [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"])

  log "zip should use the specified function to zip two lists together"
  assert $ zip (toList [1, 2, 3]) (toList ["a", "b", "c"]) == toList [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"]

  log "unzip should deconstruct a list of tuples into a tuple of lists"
  assert $ unzip (toList [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"]) == Tuple (toList [1, 2, 3]) (toList ["a", "b", "c"])

  log "foldM should perform a fold using a monadic step function"
  assert $ foldM (\x y -> Just (x + y)) 0 (range 1 10) == Just 55
  assert $ foldM (\_ _ -> Nothing) 0 (range 1 10) == Nothing

  log "foldl should be stack-safe"
  void $ pure $ foldl (+) 0 $ range 1 100000

  log "foldMap should be stack-safe"
  void $ pure $ foldMap Additive $ range 1 100000

  log "foldMap should be left-to-right"
  assert $ foldMap show (range 1 5) == "12345"

  log "unfoldr should be stack-safe"
  void $ pure $ length $ Data.Unfoldable.replicate 100000 1

  log "unfoldr should maintain order"
  assert $ (1..5) == Data.Unfoldable.unfoldr step 1

  -- log "can find the first 10 primes using lazy lists"
  -- let eratos :: L.List Number -> L.List Number
  --     eratos xs = Control.Lazy.defer \_ ->
  --       case L.uncons xs of
  --         Nothing -> L.nil
  --         Just (Tuple p xs) -> p `L.cons` eratos (L.filter (\x -> x % p /= 0) xs)

  --     upFrom = L.iterate (1 +)

  --     primes = eratos $ upFrom 2
  -- assert $ L.fromList (L.take 10 primes) == [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

step :: Int -> Maybe (Tuple Int Int)
step 6 = Nothing
step n = Just (Tuple n (n + 1))

nil :: List Int
nil = Nil

odd :: Int -> Boolean
odd n = n `mod` 2 /= zero

doubleAndOrig :: Int -> List Int
doubleAndOrig x = Cons (x * 2) (Cons x Nil)
