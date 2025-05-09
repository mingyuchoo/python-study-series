module DatabaseSpec where

import Test.Hspec
import Database
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "Database operations" $ do
    it "should create a new empty database" $ do
      db <- newDB
      map <- liftIO $ readIORef db
      map `shouldBe` Map.empty
    
    it "should insert and retrieve a value" $ do
      db <- newDB
      insert db "key1" "value1"
      result <- lookup db "key1"
      result `shouldBe` Just "value1"
    
    it "should return Nothing when looking up a non-existent key" $ do
      db <- newDB
      result <- lookup db "nonexistent"
      result `shouldBe` Nothing
    
    it "should delete a value" $ do
      db <- newDB
      insert db "key1" "value1"
      delete db "key1"
      result <- lookup db "key1"
      result `shouldBe` Nothing
    
    it "should update an existing value" $ do
      db <- newDB
      insert db "key1" "value1"
      success <- update db "key1" (\_ -> "updated")
      result <- lookup db "key1"
      success `shouldBe` True
      result `shouldBe` Just "updated"
    
    it "should return False when updating a non-existent key" $ do
      db <- newDB
      success <- update db "nonexistent" (\_ -> "updated")
      success `shouldBe` False
    
    it "should get all values from the database" $ do
      db <- newDB
      insert db "key1" "value1"
      insert db "key2" "value2"
      result <- getAll db
      result `shouldMatchList` [("key1", "value1"), ("key2", "value2")]
