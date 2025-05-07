# Environment Configuration for Haskell Web Application

This document provides guidelines for configuring different environments for the Haskell web application.

## Environment Variables

The application uses the following environment variables:

| Variable | Description | Default | Required |
|----------|-------------|---------|----------|
| `PORT` | Port number for the web server | `8080` | No |
| `LOG_LEVEL` | Logging level (debug, info, warn, error) | `info` | No |
| `DB_HOST` | Database host address | - | Yes (if using DB) |
| `DB_PORT` | Database port | - | Yes (if using DB) |
| `DB_USER` | Database username | - | Yes (if using DB) |
| `DB_PASSWORD` | Database password | - | Yes (if using DB) |
| `DB_NAME` | Database name | - | Yes (if using DB) |

## Configuration Files

For different environments, create separate `.env` files:

### Development Environment (.env.development)

```
PORT=8080
LOG_LEVEL=debug
DB_HOST=localhost
DB_PORT=5432
DB_USER=dev_user
DB_PASSWORD=dev_password
DB_NAME=haskell_web_app_dev
```

### Testing Environment (.env.test)

```
PORT=8081
LOG_LEVEL=debug
DB_HOST=localhost
DB_PORT=5432
DB_USER=test_user
DB_PASSWORD=test_password
DB_NAME=haskell_web_app_test
```

### Production Environment (.env.production)

```
PORT=8080
LOG_LEVEL=info
DB_HOST=production-db.example.com
DB_PORT=5432
DB_USER=prod_user
DB_PASSWORD=strong_password_here
DB_NAME=haskell_web_app_prod
```

## Loading Environment Variables in Haskell

To load environment variables in your Haskell application, you can use the `envy` package:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import System.Envy
import GHC.Generics
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

data Config = Config
  { port :: Int
  , logLevel :: Text
  , dbHost :: Maybe Text
  , dbPort :: Maybe Int
  , dbUser :: Maybe Text
  , dbPassword :: Maybe Text
  , dbName :: Maybe Text
  } deriving (Generic, Show)

instance FromEnv Config where
  fromEnv = Config
    <$> envMaybe "PORT" .!= 8080
    <*> envMaybe "LOG_LEVEL" .!= "info"
    <*> envMaybe "DB_HOST"
    <*> envMaybe "DB_PORT"
    <*> envMaybe "DB_USER"
    <*> envMaybe "DB_PASSWORD"
    <*> envMaybe "DB_NAME"

loadConfig :: IO Config
loadConfig = do
  result <- decodeEnv :: IO (Either String Config)
  case result of
    Left err -> error $ "Failed to load config: " ++ err
    Right config -> return config
```

## Environment-Specific Configurations

### Development

- Verbose logging
- Local database
- Debug mode enabled
- No rate limiting

### Testing

- Isolated database
- Mock external services
- Comprehensive logging

### Staging

- Production-like environment
- Separate database from production
- Reduced logging

### Production

- Minimal logging (only errors and important info)
- Performance optimizations enabled
- Rate limiting
- HTTPS required
- Database connection pooling

## Secrets Management

For sensitive information like API keys and passwords:

1. Never commit secrets to version control
2. Use environment variables for local development
3. Use a secrets management service for production (AWS Secrets Manager, HashiCorp Vault, etc.)
4. Rotate credentials regularly

## Configuration Best Practices

1. Use a hierarchical configuration system
2. Default to secure values
3. Validate configuration at startup
4. Document all configuration options
5. Use different configurations for different environments
6. Keep sensitive information separate from application code
