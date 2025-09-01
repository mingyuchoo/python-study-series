@echo off
echo Starting Korean Document Checker Frontend...
echo.

REM Check if uv is installed
uv --version >nul 2>&1
if errorlevel 1 (
    echo Error: uv is not installed or not in PATH
    echo Please install uv first: https://docs.astral.sh/uv/getting-started/installation/
    pause
    exit /b 1
)

REM Change to frontend directory
cd /d "%~dp0frontend"

REM Install dependencies if needed
echo Installing/updating dependencies...
uv sync

REM Start the frontend application
echo.
echo Starting Streamlit application on http://localhost:8501
echo Make sure the backend server is running on http://localhost:8000
echo.
echo Press Ctrl+C to stop the application
echo.

uv run streamlit run app.py --server.port 8501

pause