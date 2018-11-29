
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Applicative
import qualified Control.Concurrent as C


newtype Inbox i = Inbox { getInbox :: C.Chan i }

newtype Actor i a = Actor { unActor :: ReaderT (Inbox i) IO a }
  deriving (Functor, Applicative, Monad)

spawn :: Actor i a -> Actor i' (Inbox i)
spawn act = Actor $ do
  inbox <- fmap Inbox (lift C.newChan)
  _ <- lift (C.forkIO (runReaderT (() <$ unActor act) inbox))
  return inbox

send :: Inbox i -> i -> Actor i' ()
send inbox msg = Actor (lift (C.writeChan (getInbox inbox) msg))

receive :: (i -> Actor i a) -> Actor i a
receive handler = Actor $ asks getInbox >>= lift . C.readChan >>= unActor . handler

run :: Actor i a -> IO a
run act = fmap Inbox C.newChan >>= runReaderT (unActor act)

io :: IO a -> Actor i a
io = Actor . lift

log :: Show a => a -> Actor i ()
log = io . print
