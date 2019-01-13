module BTree

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
fold: (b -> a -> b -> b) -> b -> BTree a -> b
fold f x Leaf = x
fold f x (Node l v r) = f (fold f x l) v (fold f x r)

-- Functor BTree where
--   map f = toTree . map f . toList


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


