import Data.List
import Data.Function

type Peg = String
type Move = (Peg, Peg)

hanoi3 :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi3 1 a b c = [(a, b)]
hanoi3 n a b c =
  stepOneMoves ++ stepTwoMoves ++ stepThreeMoves
  where
    stepOneMoves = hanoi3 (n-1) a c b
    stepTwoMoves = hanoi3 1 a b c
    stepThreeMoves = hanoi3 (n-1) c b a

hanoi :: Integer -> [Peg] -> [Move]
hanoi 1 (s:d:_) = [(s, d)]
hanoi k [s, d, t] = hanoi3 k s d t
hanoi n ps =
  minimumBy (compare `on` length) steps
  where
    steps = map (\k -> hanoi' k ps) [1..(n-1)]
    hanoi' k (s:d:t:ps) =
      step1 ++ step2 ++ step1
      where
        step1 = hanoi k (s:d:t:ps)
        step2 = hanoi (n-k) (s:d:ps)
        step3 = hanoi k (t:d:s:ps)
