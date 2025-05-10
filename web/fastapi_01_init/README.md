# fastapi-01-init

## How to install `fastapi`

```bash
uv pip install fastapi
uv pip install "uvicorn[standard]"
uv pip install mypy
```

### How to install using `requirements.txt`

```bash
cd docs
uv pip install -r requirements.txt
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
