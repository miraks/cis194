{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Data.Maybe
import Log

extractType :: [String] -> Maybe MessageType
extractType ws
  | t == "I" = Just Info
  | t == "W" = Just Warning
  | t == "E" = Just $ Error $ read $ ws !! 1
  | otherwise = Nothing
  where t = head ws

extractTimestamp :: [String] -> TimeStamp
extractTimestamp ws =
  read $ case messageType of
    Error _ -> ws !! 2
    _ -> ws !! 1
  where messageType = fromJust $ extractType ws

extractText :: [String] -> String
extractText ws =
  unwords $ case messageType of
    Error _ -> drop 3 ws
    _ -> drop 2 ws
  where messageType = fromJust $ extractType ws

parseMessage :: String -> LogMessage
parseMessage s =
  case mmt of
    Just mt -> LogMessage mt (extractTimestamp ws) (extractText ws)
    Nothing -> Unknown s
  where
    ws = words s
    mmt = extractType ws

parse :: String -> [LogMessage]
parse = map parseMessage . lines


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ ts _) t@(Node lst cm@(LogMessage _ cts _) rst)
  | ts > cts = Node lst cm (insert m rst)
  | ts < cts = Node (insert m lst) cm rst
  | otherwise = t

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lst m rst) = inOrder lst ++ [m] ++ inOrder rst


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
  map (\(LogMessage _ _ m) -> m) . filter isImportantError . inOrder . build
  where
    isImportantError (LogMessage t _ _) =
      case t of
        Error n -> n > 50
        _ -> False
