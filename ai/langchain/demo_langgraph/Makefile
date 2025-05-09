.PHONY: all clean install format cli

# Extract project name from directory path
PROJECT_NAME := $(shell basename $(CURDIR))

all: clean install format cli

clean:
	rm -rf my_reports uploads __pycache__

install:
	pip install -r requirements.txt

format:
	black .
	isort .

cli:
	python cli_main.py

web:
	streamlit run web_main.py