module Main where

import Control.Applicative
import Control.Concurrent.Actor
import Control.Monad
import Data.List
import Data.List.Split
import System.Environment

type Board = [[Char]]

master :: Behavior (Board, ActorId Board) (Either Board (ActorId Board))
master (board, top) = do
  self <- getSelf
  mapM_ (send self . Left) (update board)
  loop
  where
    isRight (Right _) = True
    isRight _ = False
    loop = do
      Left b <- receiveUntil (not . isRight)
      if isCompleted b
        then send top b
        else do
          Right aid <- receiveUntil isRight
          send aid b
          loop

worker :: Behavior (ActorId (Either Board (ActorId Board))) Board
worker aid = do
  msg <- receiveMaybe
  case msg of
    Nothing -> do
      self <- getSelf
      send aid $ Right self
      worker aid
    Just board -> do
      mapM_ (send aid . Left) (update board)
      worker aid

update :: Board -> [Board]
update board = do
  board' <- [ left ++ [left' ++ [val] ++ right'] ++ right | val <- ['1'..'9'] ]
  elim board'
  where
    (left, fit:right) = break (any (== '0')) board
    (left', _:right') = break (== '0') fit
    elim b
      | isFailed b = []
      | otherwise = [b]

isCompleted :: Board -> Bool
isCompleted = all (all (/= '0'))

isFailed :: Board -> Bool
isFailed board = anyDup [board, transpose board, box board]
  where
    duplicate [] = False
    duplicate (x:xs) = elem x xs || duplicate xs
    anyDup = any (any (duplicate . filter (/= '0')))
    box = map concat . concat . map (chunksOf 3) . transpose . map (chunksOf 3)

-- format: http://school.maths.uwa.edu.au/~gordon/sudoku17
getBoards :: FilePath -> IO [Board]
getBoards path = map (chunksOf 9) . lines <$> readFile path

main :: IO ()
main = do
  args <- getArgs
  case args of
    path : num : [] -> do
      boards <- getBoards path
      wait go (head boards, read num)
    _ -> do
      error "sudoku <filepath> <worker-number>"
  where
    go (board, num) = do
      self <- getSelf
      mas <- create master (board, self)
      replicateM_ num $ create worker mas
      answer <- receive
      liftIO $ putStrLn $ unlines answer
