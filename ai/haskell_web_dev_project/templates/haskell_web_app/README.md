# Haskell Web Application Template

This is a template for a web application written in Haskell using the Scotty web framework.

## Prerequisites

- GHC (Glasgow Haskell Compiler)
- Cabal or Stack (Haskell build tools)

## Getting Started

### Using Cabal

```bash
# Update package list
cabal update

# Build the project
cabal build

# Run the application
cabal run
```

### Using Stack

```bash
# Initialize stack project (if not already done)
stack init

# Build the project
stack build

# Run the application
stack exec haskell-web-app
```

## Project Structure

```
./
├── app.hs                  # Main application entry point
├── haskell-web-app.cabal   # Cabal configuration file
└── static/                 # Static files directory
```

## Configuration

The application reads the following environment variables:

- `PORT`: The port number to listen on (default: 8080)

## API Endpoints

- `GET /`: Home page
- `GET /api/status`: API status endpoint

## License

This project is licensed under the MIT License - see the LICENSE file for details.
