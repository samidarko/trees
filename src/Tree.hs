module Tree where

type Size = Int

data Tree v a = Leaf   v a
              | Branch v (Tree v a) (Tree v a) deriving (Show)

t =  Branch 5
  (Branch 2
    (Leaf 1 1) (Leaf 1 2))
  (Branch 3
    (Leaf 1 3) (Branch 2 (Leaf 1 4) (Leaf 1 5)))

t2 = branch (leaf 3) (branch (leaf 1) (leaf 2))

toList :: Tree v a -> [a]
toList (Leaf _ a)     = [a]
toList (Branch _ x y) = toList x ++ toList y

tag :: Tree v a -> v
tag (Leaf v _)     = v
tag (Branch v _ _) = v

thead :: Tree v a -> a
thead (Leaf _ a)     = a
thead (Branch _ x _) = thead x

leaf :: a -> Tree Size a
leaf a = Leaf 1 a

branch :: Tree Size a -> Tree Size a -> Tree Size a
branch x y = Branch (tag x + tag y) x y

(!!!) :: Tree Size a -> Int -> a
(Leaf _ a)      !!! 0 = a
(Branch _ x y)  !!! n
  | n < tag x     = x !!! n
  | otherwise     = y !!! (n - tag x)

