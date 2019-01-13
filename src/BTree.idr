module BTree

import Prelude.Algebra

public export
data BTree a = Leaf
             | Node (BTree a) a (BTree a)

export
empty: BTree a
empty = Leaf


export
insert: Ord a => a -> BTree a -> BTree a
insert x Leaf = Node Leaf x Leaf
insert x (Node l v r) = if x < v
                        then Node (insert x l) v r
                        else Node l v (insert x r)

export
max: Ord a => BTree a -> Maybe a
max Leaf = Nothing
max (Node _ v Leaf) = Just v
max (Node _ _ r) = max r

export
min: Ord a => BTree a -> Maybe a
min Leaf = Nothing
min (Node Leaf v _) = Just v
min (Node l _ _) = min l

export
popMax : Ord a => BTree a -> (BTree a, Maybe a)
popMax Leaf = (Leaf, Nothing)
popMax (Node l v Leaf) = (l, Just v)
popMax (Node l v r) = let (r', max) = popMax r in
                         (Node l v r', max)

export
popMin : Ord a => BTree a -> (BTree a, Maybe a)
popMin Leaf = (Leaf, Nothing)
popMin (Node Leaf v r) = (r, Just v)
popMin (Node l v r) = let (l', min) = popMin l in
                         (Node l' v r, min)

export
delete : Ord a => a -> BTree a -> BTree a
delete x Leaf = Leaf
delete x (Node l v r) = case compare x v of
  LT => Node (delete x l)  v r
  GT => Node l v (delete x r)
  EQ => case popMax l of
        (l', Just max) => Node l' max r
        (l', Nothing) => r -- l' = Leaf

export
fold: (b -> a -> b -> b) -> b -> BTree a -> b
fold f x Leaf = x
fold f x (Node l v r) = f (fold f x l) v (fold f x r)

-- Functor BTree where
--   map f = toTree . map f . toList


export
maxHeight : BTree a -> Integer
maxHeight t = fold (\lh,_,rh => (max lh rh) + 1) 0 t

export
minHeight : BTree a -> Integer
minHeight t = fold (\lh,_,rh => (min lh rh) + 1) 0 t

export
height : BTree a -> Integer
height = maxHeight


--   a
--  / \
-- l  b
--   / \
--  rl  rr
--
--  |
--  v
--
--     b
--    / \
--   a   r
--  / \
-- l  rl
rotateLeft: Ord a => BTree a -> BTree a
rotateLeft Leaf = Leaf
rotateLeft (Node l a Leaf) = Node l a Leaf
rotateLeft (Node l a (Node rl b rr)) = Node (Node l a rl) b rr

--     b
--    / \
--   a   r
--  / \
-- ll lr
--
--  |
--  v
--
--   a
--  / \
-- ll b
--   / \
--  lr  r

rotateRight: Ord a => BTree a -> BTree a
rotateRight Leaf = Leaf
rotateRight (Node Leaf v r) = Node Leaf v r
rotateRight (Node (Node ll a lr) b r) = Node ll a (Node lr b r)

Foldable BTree where
  foldr f init Leaf = init
  foldr f init (Node l v r) =
    let r = foldr f init r in
    let v = f v r in
    foldr f v l
  foldl f init Leaf = init
  foldl f init (Node l v r) =
    let l = foldl f init l in
    let v = f l v in
    foldl f v r



export
member: Ord a => a -> BTree a -> Bool
member x = any (x ==)


export
toList: BTree a -> List a
toList tree = foldr (::) [] tree

export
toTree: Ord a => List a -> BTree a
toTree xs = foldl (flip insert) empty xs


export
split: Ord a => a -> BTree a -> (BTree a, Maybe a, BTree a)
split x tree = fold (\l, v, r =>
  let (ll, y, lr) = l in
  let (rl, z, rr) = r in
  case compare x v of
    LT => -- z = Nothing, rl = Leaf
      (ll, y, Node lr v rr)
    EQ => -- y = Nothing, lr = Leaf, rl = Leaf, z = Nothing
      (ll, Just v, rr)
    GT => -- y = Noghing, lr = Leaf
      (Node ll v rl, z, rr)
  ) (Leaf, Nothing, Leaf) tree

export
merge: Ord a => BTree a -> BTree a -> BTree a
merge Leaf y = y
merge x Leaf = x
merge (Node l v r) t =
  let (l', _, r') = split v t in
  Node (merge l l') v (merge r r')



Ord a => Semigroup (BTree a) where
  (<+>) = merge

Ord a => Monoid (BTree a) where
  neutral = empty
