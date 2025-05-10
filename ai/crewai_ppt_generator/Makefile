.PHONY: all clean install format dev server

# Extract project name from directory path
PROJECT_NAME := $(shell basename $(CURDIR))

all: clean install format dev

clean:
	rm -rf my_reports uploads __pycache__

install:
	uv pip install -r requirements.txt

format:
	black .
	isort .

dev:
	python web_app.py

server:
	uvicorn web_app:app --host 0.0.0.0 --port 8080 --reload
