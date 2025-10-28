.PHONY: all clean install format backend frontend

# Extract project name from directory path
PROJECT_NAME := $(shell basename $(CURDIR))

all: clean install format backend frontend

clean:
	rm -rf my_reports uploads __pycache__

install:
	uv venv --python 3.12
	.\.venv\Scripts\activate.ps1
	uv pip install -r requirements.txt

format:
	black .
	isort .
	taplo format .

backend:
	uv run uvicorn backend.main:backend --reload

frontend:
	uv run streamlit run frontend/app.py --logger.level debug
