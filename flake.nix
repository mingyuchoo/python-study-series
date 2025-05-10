{
  description = "A basic devShell using flake-utils for Python3";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in
        rec {
          devShell = pkgs.mkShell {
            buildInputs = with pkgs; [
              direnv
              python311Full
              python311Packages.pip
              python311Packages.virtualenv
            ];
            shellHook = ''
              export LANG=C.UTF-8
              export EDITOR=emacs
              eval "$(direnv hook bash)"
              python -m venv $HOME/.venv
              source $PWD/venv/bin/activate
              uv pip install \
                     --requirement requirements.txt \
                     --trusted-host pypi.org \
                     --trusted-host files.pythonhosted.org
              echo "Welcome to nix flake for Python3"
            '';
          };
        }
    );
}
