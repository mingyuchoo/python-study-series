module Main where

import Web.Scotty
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Data.Aeson (object, (.=))
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)

-- | Main entry point
main :: IO ()
main = do
    port <- lookupEnv "PORT"
    let portNum = read $ fromMaybe "8080" port
    
    putStrLn $ "Starting server on port " ++ show portNum
    
    scotty portNum $ do
        -- Middleware
        middleware logStdoutDev
        middleware $ staticPolicy (addBase "static")
        
        -- Routes
        get "/" $ do
            html $ "<h1>Welcome to Haskell Web App</h1><p>This is a template for a Haskell web application.</p>"
        
        get "/api/status" $ do
            json $ object ["status" .= ("ok" :: String), "message" .= ("Server is running" :: String)]
        
        -- Catch-all route
        notFound $ do
            html "<h1>404 - Not Found</h1>"
