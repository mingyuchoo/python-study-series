#!/bin/bash

set -e

echo "Installing Haskell dependencies for the web application template..."

# Check if cabal is installed
if ! command -v cabal &> /dev/null; then
    echo "Cabal is not installed. Would you like to install GHCup (Haskell toolchain installer)? (y/n)"
    read -r answer
    if [[ "$answer" == "y" ]]; then
        echo "Installing GHCup..."
        curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
        source ~/.ghcup/env
    else
        echo "Please install GHC and Cabal manually and try again."
        echo "Visit: https://www.haskell.org/ghcup/"
        exit 1
    fi
fi

# Update cabal package list
echo "Updating cabal package list..."
cabal update

# Install dependencies
echo "Installing dependencies..."
cabal install --lib scotty
cabal install --lib wai-middleware-static
cabal install --lib wai-extra
cabal install --lib aeson
cabal install --lib text
cabal install --lib hspec
cabal install --lib hspec-wai
cabal install --lib QuickCheck
cabal install --lib http-types

echo "Dependencies installed successfully!"
echo "You can now build the project with: cabal build"
