import Control.Arrow
import Data.List

sieveSundaram :: Integer -> [Integer]
sieveSundaram =
  map (\x -> 2*x+1) . (uncurry (\\)) . (toN &&& except)
  where
    toN n = [1..n]
    except n = [i+j+2*i*j | let n' = fromIntegral n,
                            i <- [1..floor (sqrt (n'/2))],
                            let i' = fromIntegral i,
                            j <- [i..floor ((n'-i')/(2*i'+1))]]
