module Database where

import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.IO.Class (MonadIO, liftIO)

-- | Simple in-memory database type
type InMemoryDB a = IORef (Map String a)

-- | Create a new empty database
newDB :: IO (InMemoryDB a)
newDB = newIORef Map.empty

-- | Insert a value into the database
insert :: MonadIO m => InMemoryDB a -> String -> a -> m ()
insert db key value = liftIO $ do
    modifyIORef db (Map.insert key value)

-- | Lookup a value in the database
lookup :: MonadIO m => InMemoryDB a -> String -> m (Maybe a)
lookup db key = liftIO $ do
    map <- readIORef db
    return $ Map.lookup key map

-- | Delete a value from the database
delete :: MonadIO m => InMemoryDB a -> String -> m ()
delete db key = liftIO $ do
    modifyIORef db (Map.delete key)

-- | Get all values from the database
getAll :: MonadIO m => InMemoryDB a -> m [(String, a)]
getAll db = liftIO $ do
    map <- readIORef db
    return $ Map.toList map

-- | Update a value in the database if it exists
update :: MonadIO m => InMemoryDB a -> String -> (a -> a) -> m Bool
update db key f = liftIO $ do
    map <- readIORef db
    case Map.lookup key map of
        Nothing -> return False
        Just value -> do
            modifyIORef db (Map.insert key (f value))
            return True
