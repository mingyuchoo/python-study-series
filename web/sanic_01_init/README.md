# sanic-01-init

## Prerequisites

```bash
python -m venv $HOME/.venv
source $HOME/.venv/bin/activate # add to .bashrc
uv pip install --upgrade pip setuptools
uv pip install poetry
```

## How to create a project

```bash
poetry new {project-name}
cd {project-name}
```

## How to add essential dependencies

```bash
poetry add sanic
poetry add sanic-ext
poetry add sanic-testing
```

## How to add dev dependencies

```bash
poetry add black     # 코드 포매팅 도구
poetry add flake8    # 코드 린트 도구
poetry add mypy      # 정적 타입 검사 도구
poetry add pydantic  # 데이터 유효성 검사 도구
poetry add pytest    # 단위 테스트 도구
poetry add coverage  # 테스트 커버리지 도구
```

## How to run server

```bash
# sanic <package_name>.<module_name>.app

$ sanic sanic_01_init.server.app
```

## How to check the results

```bash
curl http://localhost:8000
```

## How to check document

connect to `http://localhost:8000/docs/`

## References

- <https://python-poetry.org/>
- <https://sanic.dev/en/>
