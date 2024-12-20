<p align="center">
  <a href="https://github.com/mingyuchoo/python-study-series/blob/main/LICENSE"><img alt="license" src="https://img.shields.io/github/license/mingyuchoo/python-study-series"/></a>
  <a href="https://github.com/mingyuchoo/python-study-series/issues"><img alt="Issues" src="https://img.shields.io/github/issues/mingyuchoo/python-study-series?color=appveyor" /></a>
  <a href="https://github.com/mingyuchoo/python-study-series/pulls"><img alt="GitHub pull requests" src="https://img.shields.io/github/issues-pr/mingyuchoo/python-study-series?color=appveyor" /></a>
</p>

# python-study-series

## Package Managers

### UV

#### Install UV

```bash
curl -LsSf https://astral.sh/uv/install.sh | sh
# or
cargo install --git https://github.com/astral-sh/uv uv
```

#### Create a project

```bash
uv init {project_name}
cd {project_name}
```
#### Add packages current project

```bash
uv add {package_name}
uv run {pacage_name} check
```

#### Install tools

```bash
uv tool install {tool_name}
```

#### Install multiple Python versions

```bash
uv python install 3.10 3.11 3.12
```

#### Create venv with a specific Python version

```bash
uv venv --python 3.12.0
```

#### Use a specific Python version in the current directory

```bash
uv python pin 3.11
```
#### Run a script

```bash
uv run {script_name}.py
```

#### Install packages again

```bash
uv pip install .
```

### PDM

Install PDM for Python package management

```bash
curl -sSL https://pdm-project.org/install-pdm.py | python3 -
```

#### Hwo to create a Python project with PDM

```bash
source $HOME/.local/share/pdm/venv/bin/activate
mkdir {project-name}
cd {project-name}
pdm init
pdm add behave
pdm add coverage
pdm add pydantic
pdm add python-lsp-server
pdm add pyright
pdm add pylint
pdm add mypy
```

#### How to create a Python project with PIP on Nix
 
```bash
nix-shell
mkdir {project-name}
cd {project-name}
python -m venv $HOME/.venv
source $HOME/.venv/bin/activate
python -m pip install behave
python -m pip install coverage
python -m pip install pydantic
python -m pip install python-lsp-server
python -m pip install pyright
python -m pip install pylint
python -m pip install mypy
pythom -m list
python -m pip freeze > requirements.txt
```

## References

- https://docs.astral.sh/uv/
- https://github.com/pdm-project/pdm
