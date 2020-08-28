module Graze.Util
    ( readFrom
    , writeTo
    , writeManyTo
    ) where

import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan)
import Data.Foldable                (traverse_)
import Control.Monad.IO.Class       (MonadIO (liftIO))


readFrom :: MonadIO m => TChan a -> m a
readFrom = liftIO . atomically . readTChan

writeTo :: MonadIO m => TChan a -> a -> m ()
writeTo chan x = liftIO . atomically . writeTChan chan $ x

writeManyTo :: (MonadIO m, Traversable f) => TChan a -> f a -> m ()
writeManyTo chan xs = liftIO . atomically . traverse_ (writeTChan chan) $ xs

