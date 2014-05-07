xor :: [Bool] -> Bool
xor =
  foldr xor' False
  where
    xor' True True = False
    xor' False False = False
    xor' _ _ = True

xor' :: [Bool] -> Bool
xor' = odd . length . filter id

map' :: (a -> b) -> [a] -> [b]
map' f =
  foldr g []
  where g x acc = f x : acc

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)
