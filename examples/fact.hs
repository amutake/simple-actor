module Main where

import Control.Applicative ((<$>))
import System.Environment (getArgs)

import Control.Concurrent.Actor

fact :: Behavior () (Integer, ActorId Integer)
fact () = do
  (val, cust) <- receive
  if val == 0
    then send cust 1
    else do
      cont <- create factCont (val, cust)
      self <- getSelf
      send self (val - 1, cont)
      fact ()

factCont :: Behavior (Integer, ActorId Integer) Integer
factCont (val, cust) = do
  arg <- receive
  send cust (val * arg)

main :: IO ()
main = do
  n <- read . head <$> getArgs
  if n < 0
    then putStrLn "Please input a positive number."
    else do
      factA <- createIO fact ()
      wait go (factA, n)
  where
    go (f, n) = do
      self <- getSelf
      send f (n, self)
      result <- receive
      liftIO $ print result
