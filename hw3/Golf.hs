module Gold where

import Control.Arrow
import Data.Function
import Data.List

skips :: [a] -> [[a]]
skips xs =
  map (map fst . flip everyN xs) [1..length xs]
  where
    withIndexes = flip zip [1..]
    everyN n = filter (\(x,i) -> i `mod` n == 0) . withIndexes

localMaxima :: [Integer] -> [Integer]
localMaxima =
  map (!! 1) . filter isLocalMax . conses 3
  where
    conses n xs
      | length xs <= n = [xs]
      | otherwise = take n xs : conses n (tail xs)
    isLocalMax [a, b, c] = b > a && b > c
    isLocalMax _ = False

histogram :: [Integer] -> String
histogram xs =
  top ++ "\n" ++ bottom
  where
    emptyGroups = map (id &&& const 0) [0..9]
    groups = map (head &&& length) $ group $ sort xs
    maxHeight = maximum $ map snd groups
    columns = sort $ unionBy (\(x,_) (y,_) -> x == y) groups emptyGroups
    buildColumn (_,c) = reverse $ take maxHeight $ replicate c '*' ++ repeat ' '
    top = intercalate "\n" $ transpose $ map buildColumn columns
    bottom = replicate 10 '=' ++ "\n" ++ ['0'..'9']
