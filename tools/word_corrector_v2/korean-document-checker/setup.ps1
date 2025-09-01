Write-Host "Korean Document Checker - Quick Setup" -ForegroundColor Green
Write-Host "=====================================" -ForegroundColor Green
Write-Host ""

# Check if uv is installed
try {
    $uvVersion = uv --version 2>$null
    if ($LASTEXITCODE -eq 0) {
        Write-Host "uv is already installed: $uvVersion" -ForegroundColor Green
    }
} catch {
    Write-Host "Installing uv..." -ForegroundColor Yellow
    try {
        Invoke-RestMethod https://astral.sh/uv/install.ps1 | Invoke-Expression
        Write-Host "uv installed successfully!" -ForegroundColor Green
        Write-Host "Please restart your terminal and run this script again." -ForegroundColor Yellow
        Read-Host "Press Enter to exit"
        exit 0
    } catch {
        Write-Host "Failed to install uv. Please install manually." -ForegroundColor Red
        Read-Host "Press Enter to exit"
        exit 1
    }
}

Write-Host ""

# Setup backend
Write-Host "Setting up backend..." -ForegroundColor Yellow
$backendPath = Join-Path $PSScriptRoot "backend"
Push-Location $backendPath

try {
    uv sync
    if ($LASTEXITCODE -ne 0) {
        throw "Backend setup failed"
    }
    Write-Host "Backend setup complete!" -ForegroundColor Green
} catch {
    Write-Host "Failed to setup backend dependencies" -ForegroundColor Red
    Pop-Location
    Read-Host "Press Enter to exit"
    exit 1
}

Pop-Location
Write-Host ""

# Setup frontend
Write-Host "Setting up frontend..." -ForegroundColor Yellow
$frontendPath = Join-Path $PSScriptRoot "frontend"
Push-Location $frontendPath

try {
    uv sync
    if ($LASTEXITCODE -ne 0) {
        throw "Frontend setup failed"
    }
    Write-Host "Frontend setup complete!" -ForegroundColor Green
} catch {
    Write-Host "Failed to setup frontend dependencies" -ForegroundColor Red
    Pop-Location
    Read-Host "Press Enter to exit"
    exit 1
}

Pop-Location

# Check for .env file
$envPath = Join-Path $backendPath ".env"
if (-not (Test-Path $envPath)) {
    Write-Host ""
    Write-Host "WARNING: .env file not found!" -ForegroundColor Red
    Write-Host "Please create backend/.env file with your Azure OpenAI configuration:" -ForegroundColor Yellow
    Write-Host ""
    Write-Host "AZURE_OPENAI_ENDPOINT=https://your-resource.openai.azure.com/" -ForegroundColor Cyan
    Write-Host "AZURE_OPENAI_API_KEY=your-api-key-here" -ForegroundColor Cyan
    Write-Host "AZURE_OPENAI_API_VERSION=2024-12-01-preview" -ForegroundColor Cyan
    Write-Host "AZURE_OPENAI_DEPLOYMENT_NAME=gpt-4-deployment" -ForegroundColor Cyan
    Write-Host "MAX_FILE_SIZE=10485760" -ForegroundColor Cyan
    Write-Host "TEMP_FILE_RETENTION_HOURS=1" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "See README.md for detailed setup instructions." -ForegroundColor Yellow
    Write-Host ""
}

Write-Host ""
Write-Host "Setup completed successfully!" -ForegroundColor Green
Write-Host ""
Write-Host "To start the application:" -ForegroundColor Yellow
Write-Host "1. Run start_backend.ps1" -ForegroundColor White
Write-Host "2. In another terminal, run start_frontend.ps1" -ForegroundColor White
Write-Host "3. Open http://localhost:8501 in your browser" -ForegroundColor White
Write-Host ""
Read-Host "Press Enter to exit"