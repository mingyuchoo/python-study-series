module Json where

import Data.Aeson (ToJSON, FromJSON, encode, decode, object, (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Web.Scotty (ActionM, json, status, body)
import Network.HTTP.Types.Status (status200, status400, status404, status500)
import Control.Monad.IO.Class (liftIO)

-- | Helper function to send a JSON response with a success status
sendSuccessJSON :: ToJSON a => a -> ActionM ()
sendSuccessJSON value = do
    status status200
    json $ object [
        "success" .= True,
        "data" .= value
      ]

-- | Helper function to send a JSON error response
sendErrorJSON :: Int -> Text -> ActionM ()
sendErrorJSON code message = do
    status $ toStatus code
    json $ object [
        "success" .= False,
        "error" .= object [
            "code" .= code,
            "message" .= message
          ]
      ]
  where
    toStatus 400 = status400
    toStatus 404 = status404
    toStatus _   = status500

-- | Helper function to parse JSON request body
parseJSONBody :: FromJSON a => ActionM (Either Text a)
parseJSONBody = do
    b <- body
    case decode b of
        Nothing -> return $ Left "Invalid JSON format"
        Just value -> return $ Right value

-- | Helper function to handle JSON parsing and send appropriate response
withJSONBody :: FromJSON a => (a -> ActionM ()) -> ActionM ()
withJSONBody handler = do
    result <- parseJSONBody
    case result of
        Left err -> sendErrorJSON 400 err
        Right value -> handler value
