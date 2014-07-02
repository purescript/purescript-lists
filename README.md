# Module Documentation

## Module Data.List

### Types

    data List a where
      Nil :: List a
      Cons :: a -> List a -> List a


### Type Class Instances

    instance alternativeList :: Alternative List

    instance applicativeList :: Applicative List

    instance applyList :: Apply List

    instance bindList :: Bind List

    instance eqList :: (Eq a) => Eq (List a)

    instance foldableList :: Foldable List

    instance functorList :: Functor List

    instance monadList :: Monad List

    instance monoidList :: Monoid (List a)

    instance ordList :: (Ord a) => Ord (List a)

    instance semigroupList :: Semigroup (List a)

    instance showList :: (Show a) => Show (List a)

    instance traversableList :: Traversable List


### Values

    (!) :: forall a. List a -> Number -> Maybe a

    fromList :: forall a. [a] -> List a