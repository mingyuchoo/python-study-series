@echo off
echo Starting Korean Document Checker Backend...
echo.

REM Check if uv is installed
uv --version >nul 2>&1
if errorlevel 1 (
    echo Error: uv is not installed or not in PATH
    echo Please install uv first: https://docs.astral.sh/uv/getting-started/installation/
    pause
    exit /b 1
)

REM Check if .env file exists
if not exist ".env" (
    echo Warning: .env file not found
    echo Please create .env file with Azure OpenAI configuration
    echo See README.md for details
    echo.
)

REM Change to backend directory
cd /d "%~dp0backend"

REM Install dependencies if needed
echo Installing/updating dependencies...
uv sync

REM Start the backend server
echo.
echo Starting FastAPI server on http://localhost:8000
echo API documentation available at http://localhost:8000/docs
echo.
echo Press Ctrl+C to stop the server
echo.

uv run uvicorn app.main:app --reload --host 0.0.0.0 --port 8000

pause