# fastapi-01-init

## How to install `fastapi`

```bash
pip install fastapi
pip install "uvicorn[standard]"
pip install mypy
```

### How to install using `requirements.txt`

```bash
cd docs
pip install -r requirements.txt
```

### How to generate `requirements.txt`

```bash
cd docs
pip freeze > requirements.txt
```

## How to check data type

```bash
mypy main.py
```

## How to run

```bash
uvicorn main:app --reload
```
