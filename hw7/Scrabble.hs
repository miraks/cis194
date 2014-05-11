{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid

newtype Score = Score { getScore :: Int }
  deriving (Eq, Ord, Num)

instance Show Score where
  show (Score n) = "Score " ++ show n

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score c =
  Score $ case score' c of
    (Just (_,n)) -> n
    Nothing -> 0
  where
    scores = [("aeilnorstu", 1), ("dg", 2), ("bcmp", 3), ("fhvwy", 4),
              ("k", 5), ("jx", 8), ("qz", 10)]
    score' c = find (elem (toLower c) . fst) scores

scoreString :: String -> Score
scoreString = mconcat . map score
