type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c =
  stepOneMoves ++ stepTwoMoves ++ stepThreeMoves
  where
    stepOneMoves = hanoi (n-1) a c b
    stepTwoMoves = hanoi 1 a b c
    stepThreeMoves = hanoi (n-1) c b a
