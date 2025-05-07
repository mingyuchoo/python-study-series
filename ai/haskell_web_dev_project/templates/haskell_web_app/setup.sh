#!/bin/bash

set -e

echo "Setting up Haskell Web Application..."

# Check if cabal is installed
if ! command -v cabal &> /dev/null; then
    echo "Cabal is not installed. Please install GHC and Cabal first."
    echo "Visit: https://www.haskell.org/ghcup/"
    exit 1
fi

# Update cabal package list
echo "Updating cabal package list..."
cabal update

# Install dependencies
echo "Installing dependencies..."
cabal build --only-dependencies

# Build the project
echo "Building the project..."
cabal build

echo "Setup completed successfully!"
echo "To run the application, use: cabal run"
echo "The application will be available at: http://localhost:8080"
