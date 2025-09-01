#!/bin/bash

echo "Starting Korean Document Checker Backend..."
echo

# Check if uv is installed
if ! command -v uv &> /dev/null; then
    echo "Error: uv is not installed or not in PATH"
    echo "Please install uv first: https://docs.astral.sh/uv/getting-started/installation/"
    exit 1
fi

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Check if .env file exists
if [ ! -f "$SCRIPT_DIR/backend/.env" ]; then
    echo "Warning: .env file not found in backend directory"
    echo "Please create .env file with Azure OpenAI configuration"
    echo "See README.md for details"
    echo
fi

# Change to backend directory
cd "$SCRIPT_DIR/backend"

# Install dependencies if needed
echo "Installing/updating dependencies..."
uv sync

# Start the backend server
echo
echo "Starting FastAPI server on http://localhost:8000"
echo "API documentation available at http://localhost:8000/docs"
echo
echo "Press Ctrl+C to stop the server"
echo

uv run uvicorn app.main:app --reload --host 0.0.0.0 --port 8000