module Data.List.Lazy
  ( List(..)
  , fromArray
  , toArray
  , (!)
  , drop
  , take
  , length
  , filter
  , mapMaybe
  , catMaybes
  , head
  , tail
  , last
  , init
  , zipWith
  , null
  , span
  , group
  , groupBy
  , (\\)
  , insert
  , insertBy
  , insertAt
  , iterate
  , delete
  , deleteBy
  , deleteAt
  , alterAt
  , repeat
  , reverse
  , nub
  , nubBy
  , intersect
  , intersectBy
  , union
  , unionBy) where

  import Data.Lazy
  import Data.Array (map)
  import Data.Maybe
  import Data.Tuple
  import Data.Monoid
  import Data.Foldable
  import Data.Unfoldable
  import Data.Traversable

  import Control.Alt
  import Control.Plus
  import Control.Alternative
  import Control.MonadPlus

  import Test.QuickCheck
  import Test.QuickCheck.Gen

  import qualified Data.String as S

  -- | This needs a big redesign. Most functions are strict or unnecessarily strict
  -- | because `List` is currently strict in its head.
  data List a = Nil | Cons a (Lazy (List a))

  instance showList :: (Show a) => Show (List a) where
    show l = "List (fromArray (" ++ show (toArray l) ++ "))"

  instance eqList :: (Eq a) => Eq (List a) where
    (==) Nil         Nil         = true
    (==) (Cons x xs) (Cons y ys) = x == y && (force xs) == (force ys)
    (==) _           _           = false

    (/=) l           l'          =  not (l == l')

  instance semigroupList :: Semigroup (List a) where
    (<>) xs ys = xs <.> defer \_ -> ys

  instance ordList :: (Ord a) => Ord (List a) where
    compare Nil Nil = EQ
    compare Nil _   = LT
    compare _   Nil = GT
    compare (Cons x xs) (Cons y ys) = 
      case compare x y of
        EQ -> compare (force xs) (force ys)
        other -> other

  instance monoidList :: Monoid (List a) where
    mempty = Nil
    
  instance functorList :: Functor List where
    (<$>) f Nil = Nil
    (<$>) f (Cons h t) = Cons (f h) (((<$>) f) <$> t)

  instance foldableList :: Foldable List where
    -- foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
    foldr _ b Nil = b
    foldr o b (Cons a as) = a `o` foldr o b (force as)

    -- foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
    foldl _ b Nil = b
    foldl o b (Cons a as) = foldl o (b `o` a) (force as)

    -- foldMap :: forall a m. (Monoid m) => (a -> m) -> f a -> m 
    foldMap _ Nil = mempty
    foldMap f (Cons x xs) = f x <> foldMap f (force xs)

  instance unfoldableList :: Unfoldable List where
    -- unfoldr :: forall a b. (b -> Maybe (Tuple a b)) -> b -> List a
    unfoldr f b = go (f b)
      where
      go Nothing = Nil
      go (Just (Tuple a b)) = Cons a (defer \_ -> (go (f b)))

  instance traversableList :: Traversable List where
    -- traverse :: forall a b m. (Applicative m) => (a -> m b) -> t a -> m (t b)
    traverse _ Nil = pure Nil
    traverse f (Cons a as) = Cons <$> f a <*> (pure <$> traverse f (force as))

    -- sequence :: forall a m. (Applicative m) => t (m a) -> m (t a)   
    sequence Nil = pure Nil
    sequence (Cons a as) = Cons <$> a <*> (pure <$> sequence (force as))

  instance applyList :: Apply List where
    (<*>) = ap

  instance applicativeList :: Applicative List where
    pure x = Cons x (defer \_ -> Nil)

  instance bindList :: Bind List where
    (>>=) Nil _ = Nil
    (>>=) (Cons x xs) f = f x <.> ((\xs' -> xs' >>= f) <$> xs)

  instance monadList :: Monad List

  instance altList :: Alt List where
  (<|>) = (<>)

  instance plusList :: Plus List where
    empty = Nil

  instance alternativeList :: Alternative List 

  instance monadPlusList :: MonadPlus List

  instance arbitraryList :: (Arbitrary a) => Arbitrary (List a) where
    arbitrary = fromArray <$> arbitrary

  (<.>) :: forall a. List a -> Lazy (List a) -> List a
  (<.>) Nil         ys = force ys
  (<.>) (Cons x xs) ys = Cons x ((\xs' -> xs' <.> ys) <$> xs)

  singleton :: forall a. a -> List a
  singleton a = Cons a (pure Nil)

  fromArray :: forall a. [a] -> List a
  fromArray [] = Nil
  fromArray (x:xs) = Cons x (defer \_ -> fromArray xs)

  toArray :: forall a. List a -> [a]
  toArray Nil = []
  toArray (Cons x xs) = x : toArray (force xs)

  infix 4 !

  (!) :: forall a. List a -> Number -> Maybe a
  (!) Nil _ = Nothing
  (!) (Cons a _) 0 = Just a
  (!) (Cons _ as) i = (force as) ! i - 1

  drop :: forall a. Number -> List a -> List a
  drop 0 xs = xs
  drop _ Nil = Nil
  drop n (Cons x xs) = drop (n - 1) (force xs)

  take :: forall a. Number -> List a -> List a
  take 0 _ = Nil
  take _ Nil = Nil
  take n (Cons x xs) = Cons x (take (n - 1) <$> xs)

  length :: forall a. List a -> Number
  length Nil = 0
  length (Cons _ xs) = 1 + length (force xs)

  filter :: forall a. (a -> Boolean) -> List a -> List a
  filter _ Nil = Nil
  filter p (Cons x xs) | p x = Cons x (filter p <$> xs)
  filter p (Cons _ xs) = filter p (force xs)

  mapMaybe :: forall a b. (a -> Maybe b) -> List a -> List b
  mapMaybe _ Nil = Nil
  mapMaybe f (Cons x xs) =
    case f x of
      Nothing -> mapMaybe f (force xs)
      Just y -> Cons y (mapMaybe f <$> xs)

  catMaybes :: forall a. List (Maybe a) -> List a
  catMaybes = mapMaybe id

  head :: forall a. List a -> Maybe a
  head Nil = Nothing
  head (Cons x _) = Just x

  tail :: forall a. List a -> Maybe (List a)
  tail Nil = Nothing
  tail (Cons _ xs) = Just (force xs)

  uncons :: forall a. List a -> Maybe (Tuple a (List a))
  uncons Nil = Nothing
  uncons (Cons x xs) = Just $ Tuple x (force xs)

  last :: forall a. List a -> Maybe a
  last (Cons x xs)  = case force xs of
                        Nil -> Just x 
                        xs  -> last xs
  last _            = Nothing

  init :: forall a. List a -> Maybe (List a)
  init (Cons x xs)  = case force xs of 
                        Nil -> Just Nil 
                        xs  -> Cons x <$> (pure <$> init xs)
  init _            = Nothing

  zipWith :: forall a b c. (a -> b -> c) -> List a -> List b -> List c
  zipWith _ Nil _ = Nil
  zipWith _ _ Nil = Nil
  zipWith f (Cons a as) (Cons b bs) = Cons (f a b) (zipWith f <$> as <*> bs)

  null :: forall a. List a -> Boolean
  null Nil = true
  null _ = false

  span :: forall a. (a -> Boolean) -> List a -> Tuple (List a) (List a)
  span p (Cons x xs) | p x = 
    case span p (force xs) of
      Tuple ys zs -> Tuple (Cons x (pure ys)) zs
  span _ xs = Tuple Nil xs

  group :: forall a. (Eq a) => List a -> List (List a)
  group = groupBy (==)

  groupBy :: forall a. (a -> a -> Boolean) -> List a -> List (List a)
  groupBy _ Nil = Nil
  groupBy eq (Cons x xs) = 
    case span (eq x) (force xs) of
      Tuple ys zs -> Cons (Cons x (pure ys)) (pure $ groupBy eq zs)

  infix 5 \\

  (\\) :: forall a. (Eq a) => List a -> List a -> List a
  (\\) = foldl (flip delete)

  insert :: forall a. (Ord a) => a -> List a -> List a
  insert = insertBy compare

  insertBy :: forall a. (a -> a -> Ordering) -> a -> List a -> List a
  insertBy _ x Nil = singleton x
  insertBy cmp x ys @ (Cons y ys') =
    case cmp x y of
      GT -> Cons y (insertBy cmp x <$> ys')
      _  -> Cons x (pure ys)
 
  insertAt :: forall a. Number -> a -> List a -> Maybe (List a)
  insertAt 0 x xs = Just (Cons x (pure xs))
  insertAt n x (Cons y ys) = Cons y <<< pure <$> force (insertAt (n - 1) x <$> ys)
  insertAt _ _ _  = Nothing

  delete :: forall a. (Eq a) => a -> List a -> List a
  delete = deleteBy (==)

  deleteBy :: forall a. (a -> a -> Boolean) -> a -> List a -> List a
  deleteBy _ _ Nil = Nil
  deleteBy (==) x (Cons y ys) | x == y = force ys 
  deleteBy (==) x (Cons y ys) = Cons y (deleteBy (==) x <$> ys)
      
  deleteAt :: forall a. Number -> List a -> Maybe (List a)
  deleteAt 0 (Cons y ys) = Just $ force ys
  deleteAt n (Cons y ys) = Cons y <<< pure <$> force (deleteAt (n - 1) <$> ys)
  deleteAt _ _  = Nothing
  
  alterAt :: forall a. Number -> (a -> Maybe a) -> List a -> Maybe (List a)
  alterAt 0 f (Cons y ys) = Just $
    case f y of
      Nothing -> force ys
      Just y' -> Cons y' ys
  alterAt n f (Cons y ys) = Cons y <<< pure <$> force (alterAt (n - 1) f <$> ys)
  alterAt _ _ _  = Nothing

  reverse :: forall a. List a -> List a
  reverse l = force $ go (pure Nil) l
    where go acc Nil         = acc
          go acc (Cons x xs) = xs >>= go (pure $ Cons x acc)

  nub :: forall a. (Eq a) => List a -> List a
  nub = nubBy (==)

  nubBy :: forall a. (a -> a -> Boolean) -> List a -> List a
  nubBy _     Nil = Nil
  nubBy (==) (Cons x xs) = Cons x (nubBy (==) <$> (filter (\y -> not (x == y)) <$> xs))

  intersect :: forall a. (Eq a) => List a -> List a -> List a
  intersect = intersectBy (==)

  intersectBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a
  intersectBy _  Nil _   = Nil
  intersectBy _  _   Nil = Nil
  intersectBy eq xs  ys  = filter (\x -> any (eq x) ys) xs

  union :: forall a. (Eq a) => List a -> List a -> List a
  union = unionBy (==)

  unionBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a
  unionBy eq xs ys = xs <> foldl (flip (deleteBy eq)) (nubBy eq ys) xs

  repeat :: forall a. a -> List a
  repeat x = Cons x (defer \_ -> repeat x)

  iterate :: forall a. (a -> a) -> a -> List a
  iterate f a = Cons a (defer \_ -> iterate f (f a))