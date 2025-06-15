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
#cargo install --git https://github.com/astral-sh/uv uv
```

#### Install Python globally

```bash
# install python
uv python install 3.11 3.12 3.13

# make .venv 
uv venv --python 3.13
uv python pin 3.13

# install packages
uv pip install {package_name}
uv pip install behave
uv pip install coverage
uv pip install mypy
uv pip install notebook
uv pip install pylint
uv pip install pyright
uv pip install python-lsp-server
uv pip install jupyter
uv pip install notebook
uv pip install numpy
uv pip install pandas
uv pip install streamlit
```

#### Install Python locally

```bash
# make a project
mkdir {project_name}
cd {project_name}
uv init {project_name} --python 3.13

# make .venv
uv sync

# install packages
uv add {package_name}
uv add --dev {package_name_for_develop}
uv remove {package_name}

# build and run
uv build
uv run {script_name}.py
```

#### Install packages again

```bash
uv pip install .
# or
uv add -r requirements.txt
```

## Jupyter Notebook

### How to install Jupyter Notebook

```bash
cd
uv python install 3.13
uv venv --python 3.13
uv pip install notebook
```

### How to run Jupyter Notebook

```bash
jupyter notebook
```

## References

- https://docs.astral.sh/uv/
- https://github.com/pdm-project/pdm
