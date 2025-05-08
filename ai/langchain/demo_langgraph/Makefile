.PHONY: all clean install format run

# Extract project name from directory path
PROJECT_NAME := $(shell basename $(CURDIR))

all: clean install format run

clean:
	rm -rf my_reports uploads __pycache__

install:
	pip install -r requirements.txt

format:
	black .
	isort .

run:
	python src/imperative_main.py
