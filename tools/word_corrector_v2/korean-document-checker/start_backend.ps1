Write-Host "Starting Korean Document Checker Backend..." -ForegroundColor Green
Write-Host ""

# Check if uv is installed
try {
    $uvVersion = uv --version 2>$null
    if ($LASTEXITCODE -ne 0) {
        throw "uv not found"
    }
} catch {
    Write-Host "Error: uv is not installed or not in PATH" -ForegroundColor Red
    Write-Host "Please install uv first: https://docs.astral.sh/uv/getting-started/installation/" -ForegroundColor Yellow
    Read-Host "Press Enter to exit"
    exit 1
}

# Check if .env file exists
$backendPath = Join-Path $PSScriptRoot "backend"
$envPath = Join-Path $backendPath ".env"

if (-not (Test-Path $envPath)) {
    Write-Host "Warning: .env file not found" -ForegroundColor Yellow
    Write-Host "Please create .env file with Azure OpenAI configuration" -ForegroundColor Yellow
    Write-Host "See README.md for details" -ForegroundColor Yellow
    Write-Host ""
}

# Change to backend directory
Push-Location $backendPath

# Install dependencies if needed
Write-Host "Installing/updating dependencies..." -ForegroundColor Yellow
uv sync

# Start the backend server
Write-Host ""
Write-Host "Starting FastAPI server on http://localhost:8000" -ForegroundColor Green
Write-Host "API documentation available at http://localhost:8000/docs" -ForegroundColor Cyan
Write-Host ""
Write-Host "Press Ctrl+C to stop the server" -ForegroundColor Yellow
Write-Host ""

try {
    uv run uvicorn app.main:app --reload --host 0.0.0.0 --port 8000
} finally {
    Pop-Location
    Read-Host "Press Enter to exit"
}