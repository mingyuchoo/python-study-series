.PHONY: all clean install format dev

# Extract project name from directory path
PROJECT_NAME := $(shell basename $(CURDIR))

all: clean install format dev

clean:
	rm -rf projects __pycache__ */__pycache__

install:
	pip install -r requirements.txt

format:
	black .
	isort .

dev:
	streamlit run app.py

test:
	python -m pytest tests/
