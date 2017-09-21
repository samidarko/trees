
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

t = foldl (\acc v -> insert v acc) (Leaf) [5, 1, 6, 2, 7]

singleton x = Node x Leaf Leaf

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = singleton x
insert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (insert x left) right
  | x > a = Node a left (insert x right)


search :: (Ord a) => a -> Tree a -> Maybe a
search x Leaf = Nothing
search x (Node a left right)
  | x == a = Just x
  | x < a = search x left
  | x > a = search x right

