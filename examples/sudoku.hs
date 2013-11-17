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
  Right aid <- receiveUntil isRight
  mapM_ (send self . Left) (update board)
  loop aid
  where
    isRight (Right _) = True
    isRight _ = False
    loop aid = do
      r <- receive
      case r of
        Right aid' -> do
          loop aid'
        Left b -> do
          if isCompleted b
            then send top b
            else do
              send aid b
              loop aid

worker :: Behavior (ActorId (Either Board (ActorId Board))) Board
worker aid = do
  self <- getSelf
  send aid $ Right self
  loop2
  where
    iterateM :: Monad m => Int -> (a -> m a) -> m a -> m a
    iterateM 0 _ m = m
    iterateM n f m = iterateM (n - 1) f (m >>= f)
    loop1 board = do
      let boards = iterateM 1 update [board]
      mapM_ (send aid . Left) boards
      msg <- receiveMaybe
      case msg of
        Nothing -> do
          self <- getSelf
          send aid $ Right self
          loop2
        Just board' -> loop1 board'
    loop2 = do
      board <- receive
      loop1 board

update :: Board -> [Board]
update board = do
  if isCompleted board
    then return board
    else do
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
