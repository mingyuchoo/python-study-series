#!/bin/bash

echo "Starting Korean Document Checker Frontend..."
echo

# Check if uv is installed
if ! command -v uv &> /dev/null; then
    echo "Error: uv is not installed or not in PATH"
    echo "Please install uv first: https://docs.astral.sh/uv/getting-started/installation/"
    exit 1
fi

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Change to frontend directory
cd "$SCRIPT_DIR/frontend"

# Install dependencies if needed
echo "Installing/updating dependencies..."
uv sync

# Start the frontend application
echo
echo "Starting Streamlit application on http://localhost:8501"
echo "Make sure the backend server is running on http://localhost:8000"
echo
echo "Press Ctrl+C to stop the application"
echo

uv run streamlit run app.py --server.port 8501