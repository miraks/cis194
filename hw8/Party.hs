{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.List
import Data.Monoid
import Data.Tree
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) $ fun + empFun e

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL es1 fun1) (GL es2 fun2) = GL (es1 ++ es2) $ fun1 + fun2

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2
  | gl1 >= gl2 = gl1
  | otherwise = gl2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x ts) = f x (map (treeFold f) ts)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel b gls = (withBoss, withoutBoss)
  where
    withoutBoss = mconcat $ map (uncurry moreFun) gls
    withBoss = glCons b $ mconcat $ map snd gls

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

formatGL :: GuestList -> String
formatGL (GL es f) =
  total ++ "\n" ++ names es
  where
    total = "Total fun: " ++ show f
    names = unlines . sort . map empName

main :: IO ()
main = readFile "company.txt" >>= putStrLn . formatGL . maxFun . read
