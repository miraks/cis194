{-# OPTIONS_GHC -fno-warn-missing-methods #-}

newtype Matrix = Matrix { unMatrix :: ((Integer,Integer),(Integer,Integer)) }

instance Show Matrix where
  show (Matrix ((a,b),(c,d))) = "[[" ++ show a ++ "," ++ show b ++ "],["
                                ++ show c ++ "," ++ show d ++ "]]"

instance Num Matrix where
  (*) (Matrix ((a1,b1),(c1,d1))) (Matrix ((a2,b2),(c2,d2))) =
    Matrix (((a1*a2)+(b1*c2),(a1*b2)+(b1*d2)),
            ((c1*a2)+(d1*c2),(c1*b2)+(d1*d2)))

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = fst $ fst $ unMatrix $ Matrix ((1,1),(1,0)) ^ n
