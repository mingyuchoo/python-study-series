module AppSpec where

import Test.Hspec
import Test.Hspec.Wai
import Network.HTTP.Types.Status
import Network.Wai.Test (SResponse)
import Data.Aeson (object, (.=))
import qualified Data.ByteString.Lazy as LBS

-- Import the application (assuming it's refactored to expose the app)
import qualified Main (app)

-- | Main test spec
spec :: Spec
spec = with Main.app $ do
    describe "GET /" $ do
        it "responds with 200" $ do
            get "/" `shouldRespondWith` 200
        
        it "responds with HTML containing welcome message" $ do
            response <- get "/"
            liftIO $ do
                let body = simpleBody response
                body `shouldContain` "Welcome to Haskell Web App"
    
    describe "GET /api/status" $ do
        it "responds with 200" $ do
            get "/api/status" `shouldRespondWith` 200
        
        it "responds with JSON containing status ok" $ do
            get "/api/status" `shouldRespondWith` "{\"status\":\"ok\",\"message\":\"Server is running\"}" 
                { matchStatus = 200
                , matchContentType = "application/json"
                }
    
    describe "GET /nonexistent" $ do
        it "responds with 404" $ do
            get "/nonexistent" `shouldRespondWith` 404
