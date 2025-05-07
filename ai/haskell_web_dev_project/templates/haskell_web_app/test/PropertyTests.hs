module PropertyTests where

import Test.Hspec
import Test.QuickCheck
import Database
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (nub)

spec :: Spec
spec = do
  describe "Database property tests" $ do
    it "inserting and then looking up should return the inserted value" $ property $
      \(key :: String, value :: String) -> do
        db <- newDB
        insert db key value
        result <- lookup db key
        return $ result == Just value
    
    it "deleting a key should make it not retrievable" $ property $
      \(key :: String, value :: String) -> do
        db <- newDB
        insert db key value
        delete db key
        result <- lookup db key
        return $ result == Nothing
    
    it "updating a key should change its value" $ property $
      \(key :: String, value1 :: String, value2 :: String) -> do
        db <- newDB
        insert db key value1
        update db key (const value2)
        result <- lookup db key
        return $ result == Just value2
    
    it "getAll should return all inserted key-value pairs" $ property $
      \(kvs :: [(String, String)]) -> do
        -- Use only unique keys to avoid duplicate key issues
        let uniqueKvs = nubKeys kvs
        db <- newDB
        mapM_ (\(k, v) -> insert db k v) uniqueKvs
        result <- getAll db
        return $ sortKvs result == sortKvs uniqueKvs

-- Helper functions
nubKeys :: Ord a => [(a, b)] -> [(a, b)]
nubKeys [] = []
nubKeys ((k,v):kvs) = (k,v) : nubKeys (filter (\(k',_) -> k' /= k) kvs)

sortKvs :: Ord a => [(a, b)] -> [(a, b)]
sortKvs = sortBy (\(k1,_) (k2,_) -> compare k1 k2)
  where
    sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    sortBy _ [] = []
    sortBy cmp (x:xs) = sortBy cmp (filter (\y -> cmp y x == LT) xs) ++ [x] ++ sortBy cmp (filter (\y -> cmp y x /= LT) xs)
