
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

delete :: (Ord a) => a -> Tree a -> Tree a
delete x n@(Leaf) = n
delete x n@(Node a Leaf Leaf) = if (x == a) then Leaf else n
delete x n@(Node a left Leaf) = 
    if x == a 
       then left 
    else if x < a
       then Node a (delete x left) Leaf
    else n
delete x n@(Node a Leaf right) = 
    if x == a 
       then right 
    else if x > a
       then Node a Leaf (delete x right)
    else n
delete x n@(Node a left right) = 
    if x == a 
       then Leaf -- TODO insert left right 
    else if x < a
       then Node a (delete x left) right
    else Node a left (delete x right)

