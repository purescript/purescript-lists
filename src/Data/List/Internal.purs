module Data.List.Internal where

import Prelude

import Data.List.Types (List(..))

data Map k v
  = Leaf
  | Two (Map k v) k v (Map k v)
  | Three (Map k v) k v (Map k v) k v (Map k v)

data TreeContext k v
  = TwoLeft k v (Map k v)
  | TwoRight (Map k v) k v
  | ThreeLeft k v (Map k v) k v (Map k v)
  | ThreeMiddle (Map k v) k v k v (Map k v)
  | ThreeRight (Map k v) k v (Map k v) k v

fromZipper :: forall k v. Ord k => List (TreeContext k v) -> Map k v -> Map k v
fromZipper Nil tree = tree
fromZipper (Cons x ctx) tree =
  case x of
    TwoLeft k1 v1 right -> fromZipper ctx (Two tree k1 v1 right)
    TwoRight left k1 v1 -> fromZipper ctx (Two left k1 v1 tree)
    ThreeLeft k1 v1 mid k2 v2 right -> fromZipper ctx (Three tree k1 v1 mid k2 v2 right)
    ThreeMiddle left k1 v1 k2 v2 right -> fromZipper ctx (Three left k1 v1 tree k2 v2 right)
    ThreeRight left k1 v1 mid k2 v2 -> fromZipper ctx (Three left k1 v1 mid k2 v2 tree)

data KickUp k v = KickUp (Map k v) k v (Map k v)

-- | Insert or replace a key/value pair in a map
insert :: forall k v. Ord k => k -> v -> Map k v -> Map k v
insert k v = down Nil
  where
  comp :: k -> k -> Ordering
  comp = compare

  down :: List (TreeContext k v) -> Map k v -> Map k v
  down ctx Leaf = up ctx (KickUp Leaf k v Leaf)
  down ctx (Two left k1 v1 right) =
    case comp k k1 of
      EQ -> fromZipper ctx (Two left k v right)
      LT -> down (Cons (TwoLeft k1 v1 right) ctx) left
      _  -> down (Cons (TwoRight left k1 v1) ctx) right
  down ctx (Three left k1 v1 mid k2 v2 right) =
    case comp k k1 of
      EQ -> fromZipper ctx (Three left k v mid k2 v2 right)
      c1 ->
        case c1, comp k k2 of
          _ , EQ -> fromZipper ctx (Three left k1 v1 mid k v right)
          LT, _  -> down (Cons (ThreeLeft k1 v1 mid k2 v2 right) ctx) left
          GT, LT -> down (Cons (ThreeMiddle left k1 v1 k2 v2 right) ctx) mid
          _ , _  -> down (Cons (ThreeRight left k1 v1 mid k2 v2) ctx) right

  up :: List (TreeContext k v) -> KickUp k v -> Map k v
  up Nil (KickUp left k' v' right) = Two left k' v' right
  up (Cons x ctx) kup =
    case x, kup of
      TwoLeft k1 v1 right, KickUp left k' v' mid -> fromZipper ctx (Three left k' v' mid k1 v1 right)
      TwoRight left k1 v1, KickUp mid k' v' right -> fromZipper ctx (Three left k1 v1 mid k' v' right)
      ThreeLeft k1 v1 c k2 v2 d, KickUp a k' v' b -> up ctx (KickUp (Two a k' v' b) k1 v1 (Two c k2 v2 d))
      ThreeMiddle a k1 v1 k2 v2 d, KickUp b k' v' c -> up ctx (KickUp (Two a k1 v1 b) k' v' (Two c k2 v2 d))
      ThreeRight a k1 v1 b k2 v2, KickUp c k' v' d -> up ctx (KickUp (Two a k1 v1 b) k2 v2 (Two c k' v' d))
