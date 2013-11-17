import Control.Applicative
import Control.Concurrent.Actor
import Control.Monad
import System.Environment

main :: IO ()
main = do
  (n : t : _) <- map read <$> getArgs
  wait master (n, t)

master :: Behavior (Int, Int) ()
master (n, t) = do
  self <- getSelf
  nodeIds <- replicateM (n - 1) $ create node ()
  rootId <- create root (t, self)
  send rootId $ map Msg $ nodeIds ++ [rootId]
  msg <- receive
  liftIO $ print msg

data Msg = Msg { openMsg :: ActorId [Msg] }

node :: Behavior () [Msg]
node () = do
  (msg : msgs) <- receive
  let next = openMsg msg
  send next msgs
  loop next
  where
    loop next = do
      msg <- receive
      send next msg
      loop next

root :: Behavior (Int, ActorId ()) [Msg]
root (t, mst) = do
  (msg : msgs) <- receive
  let next = openMsg msg
  send next msgs
  [] <- receive
  send next []
  loop next t
  where
    loop _ 0 = send mst ()
    loop next t' = do
      msg <- receive
      send next msg
      loop next (t' - 1)
