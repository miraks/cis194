{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

canAttack :: Battlefield -> Bool
canAttack (Battlefield attackers _) = attackers > 1

canDefend :: Battlefield -> Bool
canDefend (Battlefield _ defenders) = defenders > 0

attackersWon :: Battlefield -> Bool
attackersWon bf = canAttack bf && not (canDefend bf)

attackersLost :: Battlefield -> Bool
attackersLost bf = not $ canAttack bf

battleFinished :: Battlefield -> Bool
battleFinished bf = attackersWon bf || attackersLost bf

battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield attackers defenders)
  | battleFinished bf = return bf
  | otherwise = do
      aDie <- replicateM aCount die
      dDie <- replicateM dCount die
      return $ battle' (sortDesc aDie) (sortDesc dDie) bf
    where
      aCount = min 3 (attackers - 1)
      dCount = min 2 defenders
      sortDesc = sortBy $ flip compare

battle' :: [DieValue] -> [DieValue] -> Battlefield -> Battlefield
battle' _ [] bf = bf
battle' [] _ bf = bf
battle' (a:as) (d:ds) (Battlefield attackers defenders)
  | a > d = nextBattle $ Battlefield attackers (defenders - 1)
  | otherwise = nextBattle $ Battlefield (attackers - 1) defenders
  where
    nextBattle = battle' as ds

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield attackers defenders)
  | battleFinished bf = return bf
  | otherwise = battle bf >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  results <- replicateM 1000 $ invade bf
  let wins = fromIntegral $ length $ filter attackersWon results
  return $ wins / 1000

exactSuccessProb :: Battlefield -> Double
exactSuccessProb bf@(Battlefield a d)
  | attackersWon bf = 1
  | attackersLost bf = 0
  | otherwise = ((exactSuccessProb $ Battlefield a (d - 2)) +
                 (exactSuccessProb $ Battlefield (a - 1) (d - 1)) +
                 (exactSuccessProb $ Battlefield (a - 2) d)) / 3
