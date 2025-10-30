Write-Host "Starting Document Review Backend Server..."

# Check if Git is available
try {
    git --version | Out-Null
    Write-Host "Git is available - Korean spell checking will be enabled"
} catch {
    Write-Host "Warning: Git not found. Korean spell checking packages may not install properly."
    Write-Host "Please install Git from https://git-scm.com/download/win"
}

# Remove existing virtual environment if it exists
if (Test-Path ".venv") {
    Write-Host "Removing existing virtual environment..."
    Remove-Item -Recurse -Force .venv
}

# Create fresh virtual environment
Write-Host "Creating virtual environment..."
uv venv --python 3.14

# Install dependencies using uv
Write-Host "Installing dependencies (including Korean packages from GitHub)..."
uv pip install -r requirements.txt

# Run the application
Write-Host "Starting FastAPI server..."
uv run python main.py
Read-Host "Press Enter to continue..."