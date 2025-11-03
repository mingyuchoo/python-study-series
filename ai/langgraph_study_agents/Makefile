.PHONY: all clean install format cli

# Extract project name from directory path
PROJECT_NAME := $(shell basename $(CURDIR))

all: clean install format cli

clean:
	rm -rf my_reports uploads __pycache__

install:
	uv pip install -r requirements.txt

format:
	uv run isort .
	uv run black .
	taplo format .

cli:
	python cli_main.py

web:
	streamlit run web_main.py --logger.level debug