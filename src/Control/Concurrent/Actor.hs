{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Concurrent.Actor
  ( ActorId
  , ActorWorld
  , Behavior
  , create
  , createIO
  , wait
  , send
  , receive
  , getSelf
  , liftIO
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( TQueue, newTQueueIO, readTQueue, writeTQueue
  , newTVarIO, readTVar, writeTVar, retry, atomically
  )
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), MonadReader (..))

type MailBox a = TQueue a

newtype ActorId r = ActorId
  { mailBox :: MailBox r
  }

newtype ActorWorld r a = ActorWorld
  { unWorld :: ReaderT (ActorId r) IO a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadReader (ActorId r)
  )

type Behavior a b = a -> ActorWorld b ()

createIO :: Behavior a r -> a -> IO (ActorId r)
createIO bdef a = do
  mbox <- newTQueueIO
  forkIO $ execWorld (ActorId mbox) $ bdef a
  return $ ActorId mbox
 where
  execWorld mbox world =
    runReaderT (unWorld world) mbox

create :: Behavior a r -> a -> ActorWorld b (ActorId r)
create bdef = liftIO . createIO bdef

send :: ActorId a -> a -> ActorWorld b ()
send actor = liftIO . atomically . writeTQueue (mailBox actor)

receive :: ActorWorld r r
receive = ask >>= liftIO . atomically . readTQueue . mailBox

getSelf :: ActorWorld r (ActorId r)
getSelf = ask

wait :: Behavior a r -> a -> IO ()
wait bdef a = do
  tvar <- newTVarIO False
  createIO after tvar
  wait' tvar
 where
  after tvar = do
    bdef a
    liftIO $ atomically $ writeTVar tvar True
  wait' tvar = atomically $ do
    r <- readTVar tvar
    case r of
      True -> return ()
      False -> retry
