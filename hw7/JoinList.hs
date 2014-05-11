{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Data.Monoid
import Buffer
import Editor
import Scrabble
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l1 l2 = Append (tag l1 `mappend` tag l2) l1 l2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

extractSize :: (Sized a) => a -> Int
extractSize = getSize . size

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ | i < 0 = Nothing
indexJ _ Empty = Nothing
indexJ i (Single _ _) | i > 0 = Nothing
indexJ _ (Single _ a) = Just a
indexJ i (Append n l1 l2)
  | i >= extractSize n = Nothing
  | s1 > i = indexJ i l1
  | otherwise = indexJ (i-s1) l2
  where s1 = extractSize $ tag l1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n l | n <= 0 = l
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty
dropJ n (Append _ l1 l2)
  | n >= s1 = dropJ (n-s1) l2
  | otherwise = dropJ n l1 +++ l2
  where s1 = extractSize $ tag l1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n l | n <= 0 = Empty
takeJ _ Empty = Empty
takeJ _ l@(Single _ _) = l
takeJ n (Append _ l1 l2)
  | n >= s1 = l1 +++ takeJ (n-s1) l2
  | otherwise = takeJ n l1
  where s1 = extractSize $ tag l1

sample :: JoinList Size Char
sample =
  Append (Size 5)
    (Append (Size 3)
      (Single (Size 1) 'a')
      (Append (Size 2)
        (Single (Size 1) 'b')
        (Single (Size 1) 'c')))
    (Append (Size 2)
      (Single (Size 1) 'd')
      (Single (Size 1) 'e'))

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Buffer (JoinList (Score, Size) String) where
  toString = concat . jlToList

  fromString = foldr1 (+++) . map (\l -> Single (scoreString l, Size 1) l) . lines

  line = indexJ

  replaceLine n s l = takeJ (n-1) l +++ fromString s +++ dropJ n l

  numLines Empty = 0
  numLines (Single (_,Size n) _) = n
  numLines (Append (_,Size n) _ _) = n

  value Empty = 0
  value (Single (Score n,_) _) = n
  value (Append (Score n,_) _ _) = n

main = runEditor editor (fromString "test" :: JoinList (Score, Size) String)
