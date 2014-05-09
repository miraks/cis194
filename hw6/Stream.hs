module Stream where

newtype Stream a = Stream { streamToList :: [a] }

instance (Show a) => Show (Stream a) where
  show =  show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat = Stream . repeat

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f = Stream . map f . streamToList

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f = Stream . iterate f

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream (x:xs)) s =
  Stream $ x : streamToList (interleaveStreams s (Stream xs))
