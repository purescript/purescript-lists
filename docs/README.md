# Module Documentation

## Module Control.Monad.ListT

### Types

    data ListT f a


### Type Class Instances

    instance altListT :: (Applicative f) => Alt (ListT f)

    instance alternativeListT :: (Monad f) => Alternative (ListT f)

    instance applicativeListT :: (Monad f) => Applicative (ListT f)

    instance applyListT :: (Monad f) => Apply (ListT f)

    instance bindListT :: (Monad f) => Bind (ListT f)

    instance functorListT :: (Functor f) => Functor (ListT f)

    instance monadListT :: (Monad f) => Monad (ListT f)

    instance monadPlusListT :: (Monad f) => MonadPlus (ListT f)

    instance monadTransListT :: MonadTrans ListT

    instance monoidListT :: (Applicative f) => Monoid (ListT f a)

    instance plusListT :: (Monad f) => Plus (ListT f)

    instance semigroupListT :: (Applicative f) => Semigroup (ListT f a)

    instance unfoldableListT :: (Monad f) => Unfoldable (ListT f)


### Values

    catMaybes :: forall f a. (Functor f) => ListT f (Maybe a) -> ListT f a

    cons' :: forall f a. (Applicative f) => Lazy a -> Lazy (ListT f a) -> ListT f a

    drop :: forall f a. (Applicative f) => Number -> ListT f a -> ListT f a

    dropWhile :: forall f a. (Applicative f) => (a -> Boolean) -> ListT f a -> ListT f a

    filter :: forall f a. (Functor f) => (a -> Boolean) -> ListT f a -> ListT f a

    foldl :: forall f a b. (Monad f) => (b -> a -> b) -> b -> ListT f a -> f b

    foldl' :: forall f a b. (Monad f) => (b -> a -> f b) -> b -> ListT f a -> f b

    fromArray :: forall f a. (Monad f) => [a] -> ListT f a

    fromEffect :: forall f a. (Applicative f) => f a -> ListT f a

    head :: forall f a. (Monad f) => ListT f a -> f (Maybe a)

    iterate :: forall f a. (Monad f) => (a -> a) -> a -> ListT f a

    mapMaybe :: forall f a b. (Functor f) => (a -> Maybe b) -> ListT f a -> ListT f b

    nil :: forall f a. (Applicative f) => ListT f a

    prepend :: forall f a. (Applicative f) => a -> ListT f a -> ListT f a

    prepend' :: forall f a. (Applicative f) => a -> Lazy (ListT f a) -> ListT f a

    repeat :: forall f a. (Monad f) => a -> ListT f a

    scanl :: forall f a b. (Monad f) => (b -> a -> b) -> b -> ListT f a -> ListT f b

    singleton :: forall f a. (Applicative f) => a -> ListT f a

    tail :: forall f a. (Monad f) => ListT f a -> f (Maybe (ListT f a))

    take :: forall f a. (Applicative f) => Number -> ListT f a -> ListT f a

    takeWhile :: forall f a. (Applicative f) => (a -> Boolean) -> ListT f a -> ListT f a

    toArray :: forall f a. (Monad f) => ListT f a -> f [a]

    uncons :: forall f a. (Monad f) => ListT f a -> f (Maybe (Tuple a (ListT f a)))

    unfold :: forall f a z. (Monad f) => (z -> f (Maybe (Tuple z a))) -> z -> ListT f a

    wrapEffect :: forall f a. (Monad f) => f (ListT f a) -> ListT f a

    wrapLazy :: forall f a. (Monad f) => Lazy (ListT f a) -> ListT f a

    zipWith :: forall f a b c. (Monad f) => (a -> b -> c) -> ListT f a -> ListT f b -> ListT f c

    zipWith' :: forall f a b c. (Monad f) => (a -> b -> f c) -> ListT f a -> ListT f b -> ListT f c


## Module Data.List

### Types

    data List a where
      Nil :: List a
      Cons :: a -> List a -> List a


### Type Class Instances

    instance altList :: Alt List

    instance alternativeList :: Alternative List

    instance applicativeList :: Applicative List

    instance applyList :: Apply List

    instance bindList :: Bind List

    instance eqList :: (Eq a) => Eq (List a)

    instance foldableList :: Foldable List

    instance functorList :: Functor List

    instance monadList :: Monad List

    instance monadPlusList :: MonadPlus List

    instance monoidList :: Monoid (List a)

    instance ordList :: (Ord a) => Ord (List a)

    instance plusList :: Plus List

    instance semigroupList :: Semigroup (List a)

    instance showList :: (Show a) => Show (List a)

    instance traversableList :: Traversable List

    instance unfoldableList :: Unfoldable List


### Values

    (!) :: forall a. List a -> Number -> Maybe a

    (\\) :: forall a. (Eq a) => List a -> List a -> List a

    alterAt :: forall a. Number -> (a -> Maybe a) -> List a -> Maybe (List a)

    catMaybes :: forall a. List (Maybe a) -> List a

    delete :: forall a. (Eq a) => a -> List a -> List a

    deleteAt :: forall a. Number -> List a -> Maybe (List a)

    deleteBy :: forall a. (a -> a -> Boolean) -> a -> List a -> List a

    drop :: forall a. Number -> List a -> List a

    filter :: forall a. (a -> Boolean) -> List a -> List a

    fromArray :: forall a. [a] -> List a

    group :: forall a. (Eq a) => List a -> List (List a)

    groupBy :: forall a. (a -> a -> Boolean) -> List a -> List (List a)

    head :: forall a. List a -> Maybe a

    init :: forall a. List a -> Maybe (List a)

    insert :: forall a. (Ord a) => a -> List a -> List a

    insertAt :: forall a. Number -> a -> List a -> Maybe (List a)

    insertBy :: forall a. (a -> a -> Ordering) -> a -> List a -> List a

    intersect :: forall a. (Eq a) => List a -> List a -> List a

    intersectBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a

    last :: forall a. List a -> Maybe a

    length :: forall a. List a -> Number

    mapMaybe :: forall a b. (a -> Maybe b) -> List a -> List b

    nub :: forall a. (Eq a) => List a -> List a

    nubBy :: forall a. (a -> a -> Boolean) -> List a -> List a

    null :: forall a. List a -> Boolean

    reverse :: forall a. List a -> List a

    span :: forall a. (a -> Boolean) -> List a -> Tuple (List a) (List a)

    tail :: forall a. List a -> Maybe (List a)

    take :: forall a. Number -> List a -> List a

    toArray :: forall a. List a -> [a]

    uncons :: forall a. List a -> Maybe (Tuple a (List a))

    union :: forall a. (Eq a) => List a -> List a -> List a

    unionBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a

    zipWith :: forall a b c. (a -> b -> c) -> List a -> List b -> List c


## Module Data.List.Lazy

### Types

    newtype LazyList a where
      LazyList :: List a -> LazyList a

    type List = L.ListT Lazy


### Type Class Instances

    instance foldableLazyList :: Foldable LazyList


### Values

    unLazyList :: forall a. LazyList a -> List a


## Module Data.List.Unsafe

### Values

    head :: forall a. List a -> a

    init :: forall a. List a -> List a

    last :: forall a. List a -> a

    tail :: forall a. List a -> List a


## Module Test.Control.Monad.ListT

### Type Class Instances

    instance arbitraryListT :: (Monad f, Arbitrary a) => Arbitrary (ListT f a)


## Module Test.Data.List

### Type Class Instances

    instance arbitraryList :: (Arbitrary a) => Arbitrary (List a)