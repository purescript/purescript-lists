# Module Documentation

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

    union :: forall a. (Eq a) => List a -> List a -> List a

    unionBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a

    zipWith :: forall a b c. (a -> b -> c) -> List a -> List b -> List c


## Module Data.List.Unsafe

### Values

    head :: forall a. List a -> a

    init :: forall a. List a -> List a

    last :: forall a. List a -> a

    tail :: forall a. List a -> List a