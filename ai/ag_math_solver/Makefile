.PHONY: all clean install format run

# Extract project name from directory path
PROJECT_NAME := $(shell basename $(CURDIR))

all: clean install format run

clean:
	rm -rf */__pycache__ .cache/ .venv

install:
	uv pip install -r requirements.txt

format:
	uv run isort .
	uv run black .
	taplo format .

cli:
	python cli_main.py

debug:
	streamlit run app.py --logger.level debug

run:
	streamlit run app.py