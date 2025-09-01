Write-Host "Starting Korean Document Checker Frontend..." -ForegroundColor Green
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

# Change to frontend directory
$frontendPath = Join-Path $PSScriptRoot "frontend"
Push-Location $frontendPath

# Install dependencies if needed
Write-Host "Installing/updating dependencies..." -ForegroundColor Yellow
uv sync

# Start the frontend application
Write-Host ""
Write-Host "Starting Streamlit application on http://localhost:8501" -ForegroundColor Green
Write-Host "Make sure the backend server is running on http://localhost:8000" -ForegroundColor Cyan
Write-Host ""
Write-Host "Press Ctrl+C to stop the application" -ForegroundColor Yellow
Write-Host ""

try {
    uv run streamlit run app.py --server.port 8501
} finally {
    Pop-Location
    Read-Host "Press Enter to exit"
}