@echo off
echo Korean Document Checker - Quick Setup
echo =====================================
echo.

REM Check if uv is installed
uv --version >nul 2>&1
if errorlevel 1 (
    echo Installing uv...
    powershell -c "irm https://astral.sh/uv/install.ps1 | iex"
    if errorlevel 1 (
        echo Failed to install uv. Please install manually.
        pause
        exit /b 1
    )
    echo uv installed successfully!
    echo Please restart your terminal and run this script again.
    pause
    exit /b 0
)

echo uv is already installed: 
uv --version
echo.

REM Setup backend
echo Setting up backend...
cd /d "%~dp0backend"
uv sync
if errorlevel 1 (
    echo Failed to setup backend dependencies
    pause
    exit /b 1
)
echo Backend setup complete!
echo.

REM Setup frontend
echo Setting up frontend...
cd /d "%~dp0..\frontend"
uv sync
if errorlevel 1 (
    echo Failed to setup frontend dependencies
    pause
    exit /b 1
)
echo Frontend setup complete!
echo.

REM Check for .env file
cd /d "%~dp0backend"
if not exist ".env" (
    echo.
    echo WARNING: .env file not found!
    echo Please create backend/.env file with your Azure OpenAI configuration:
    echo.
    echo AZURE_OPENAI_ENDPOINT=https://your-resource.openai.azure.com/
    echo AZURE_OPENAI_API_KEY=your-api-key-here
    echo AZURE_OPENAI_API_VERSION=2024-12-01-preview
    echo AZURE_OPENAI_DEPLOYMENT_NAME=gpt-4-deployment
    echo MAX_FILE_SIZE=10485760
    echo TEMP_FILE_RETENTION_HOURS=1
    echo.
    echo See README.md for detailed setup instructions.
    echo.
)

echo.
echo Setup completed successfully!
echo.
echo To start the application:
echo 1. Run start_backend.bat
echo 2. In another terminal, run start_frontend.bat
echo 3. Open http://localhost:8501 in your browser
echo.
pause