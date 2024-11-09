<p align="center">
  <a href="https://github.com/mingyuchoo/python-study-series/blob/main/LICENSE"><img alt="license" src="https://img.shields.io/github/license/mingyuchoo/python-study-series"/></a>
  <a href="https://github.com/mingyuchoo/python-study-series/issues"><img alt="Issues" src="https://img.shields.io/github/issues/mingyuchoo/python-study-series?color=appveyor" /></a>
  <a href="https://github.com/mingyuchoo/python-study-series/pulls"><img alt="GitHub pull requests" src="https://img.shields.io/github/issues-pr/mingyuchoo/python-study-series?color=appveyor" /></a>
</p>

# python-study-series

## Prerequsites

Install PDM for Python package management

```bash
curl -sSL https://pdm-project.org/install-pdm.py | python3 -
```

## Hwo to create a Python project with PDM

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

## How to create a Python project with PIP on Nix
 
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

- https://github.com/pdm-project/pdm
