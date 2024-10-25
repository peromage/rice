/* Python FHS environment.

   This is used as a "virtual" environment which allows to install user packages.
   This also supports some packages that require C compilation during installation.
*/

{ pkgs, writeScriptBin, ... }:

let
  python = let p = pkgs.pixPkgs.python; in p.override {
    userPyenvDir = ''''${XDG_DATA_HOME:-$HOME/.local/share}/${p.userPyenvDir}'';
  };

  initUserPyenvScript = writeScriptBin "init-user-pyenv.sh" ''
    set -e
    DIR="''${1:-''${USER_PYENV_DIR:?Missing env path}}"
    if python -m venv --system-site-packages --copies "$DIR"; then
      echo "Initialized user pyenv in $DIR"
    else
      echo "Failed to initialize user pyenv in $DIR"
    fi
  '';

  fhsenv = pkgs.buildFHSEnv {
    name = "python-fhs-env";
    targetPkgs = pkgs: with pkgs; [
      python
      pixPkgs.build-essential
      initUserPyenvScript
    ];

    ## See: https://nixos.wiki/wiki/Python#Emulating_virtualenv_with_nix-shell
    profile = ''
      ## Enables pip install in a virtual environment reference packages globally
      export USER_PYENV_DIR="${python.userPyenvDir}"
      export PYTHONPATH="${python.userPythonPath}:$PYTHONPATH"
      export PATH="${python.userPath}:$PATH"
      unset SOURCE_DATE_EPOCH

      if [[ -d "$USER_PYENV_DIR" ]]; then
        source "$USER_PYENV_DIR/bin/activate"
      else
        init-user-pyenv.sh "$USER_PYENV_DIR"
        source "$USER_PYENV_DIR/bin/activate"
      fi
    '';
  };

in fhsenv.env
