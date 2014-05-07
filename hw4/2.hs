data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf

insertNode :: a -> Tree a -> Tree a
insertNode x Leaf = Node 0 Leaf x Leaf
insertNode x (Node h ln y rn)
  | treeHeight ln < treeHeight rn =
    let nn = insertNode x ln
    in Node (treeHeight nn + 1) nn y rn
  | otherwise =
    let nn = insertNode x rn
    in Node (treeHeight nn + 1) ln y nn

treeHeight :: Tree a -> Integer
treeHeight Leaf = -1
treeHeight (Node h _ _ _) = h
