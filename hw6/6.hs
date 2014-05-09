{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

import Stream

x :: Stream Integer
x = Stream $ 0 : 1 : repeat 0

instance Num (Stream Integer) where
  fromInteger n = Stream $ n : repeat 0
  negate = Stream . map negate . streamToList
  (+) (Stream as) (Stream bs) =
    Stream $ zipWith (+) as bs
  (*) (Stream (a:as)) s@(Stream (b:bs)) =
    Stream $ a * b : streamToList (Stream (map (*a) bs) + Stream as * s)

instance Fractional (Stream Integer) where
  (/) (Stream (a:as)) (Stream (b:bs)) = q
    where q = Stream $ a `div` b : streamToList (Stream (map (`div` b) as) - q * Stream bs)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)
