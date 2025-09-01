Write-Host "Starting Document Review Frontend..."
uv venv --python 3.12
.\.venv\Scripts\Activate.ps1
uv pip install -r requirements.txt
streamlit run app.py
Read-Host "Press Enter to continue..."