.PHONY: all clean install format backend frontend

# Extract project name from directory path
PROJECT_NAME := $(shell basename $(CURDIR))

all: clean install format backend frontend

clean:
	rm -rf my_reports uploads __pycache__

install:
	uv pip install -r requirements.txt
	.\.venv\Scripts\activate.ps1
format:
	black .
	isort .

backend:
	uv run uvicorn backend.main:backend --reload

frontend:
	uv run streamlit run frontend/app.py --logger.level debug