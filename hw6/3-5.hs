import Data.List
import Data.Maybe
import Stream

nats :: Stream Integer
nats = Stream [0..]

ruler' :: Stream Integer
ruler' =
  Stream $ map maxPowerOf2 [1..]
  where
    maxPowerOf2 n = snd $ fromJust $ find (\(x,_) -> n `mod` x == 0)
                    $ reverse $ takeWhile ((<=n) . fst) powersOf2
    powersOf2 = zip (iterate (*2) 1) [0..]

ruler :: Stream Integer
ruler = foldr1 interleaveStreams (map streamRepeat [0..])
