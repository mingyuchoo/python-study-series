#!/bin/bash

echo "Korean Document Checker - Quick Setup"
echo "====================================="
echo

# Check if uv is installed
if ! command -v uv &> /dev/null; then
    echo "Installing uv..."
    curl -LsSf https://astral.sh/uv/install.sh | sh
    if [ $? -ne 0 ]; then
        echo "Failed to install uv. Please install manually."
        exit 1
    fi
    echo "uv installed successfully!"
    echo "Please restart your terminal and run this script again."
    exit 0
fi

echo "uv is already installed: $(uv --version)"
echo

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Setup backend
echo "Setting up backend..."
cd "$SCRIPT_DIR/backend"
uv sync
if [ $? -ne 0 ]; then
    echo "Failed to setup backend dependencies"
    exit 1
fi
echo "Backend setup complete!"
echo

# Setup frontend
echo "Setting up frontend..."
cd "$SCRIPT_DIR/frontend"
uv sync
if [ $? -ne 0 ]; then
    echo "Failed to setup frontend dependencies"
    exit 1
fi
echo "Frontend setup complete!"
echo

# Check for .env file
if [ ! -f "$SCRIPT_DIR/backend/.env" ]; then
    echo
    echo "WARNING: .env file not found!"
    echo "Please create backend/.env file with your Azure OpenAI configuration:"
    echo
    echo "AZURE_OPENAI_ENDPOINT=https://your-resource.openai.azure.com/"
    echo "AZURE_OPENAI_API_KEY=your-api-key-here"
    echo "AZURE_OPENAI_API_VERSION=2024-12-01-preview"
    echo "AZURE_OPENAI_DEPLOYMENT_NAME=gpt-4-deployment"
    echo "MAX_FILE_SIZE=10485760"
    echo "TEMP_FILE_RETENTION_HOURS=1"
    echo
    echo "See README.md for detailed setup instructions."
    echo
fi

echo
echo "Setup completed successfully!"
echo
echo "To start the application:"
echo "1. Run ./start_backend.sh"
echo "2. In another terminal, run ./start_frontend.sh"
echo "3. Open http://localhost:8501 in your browser"
echo