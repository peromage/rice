/* Python FHS environment.

   This is used as a "virtual" environment which allows to install user packages.
   This also supports some packages that require C compilation during installation.
*/

{ pkgs, writeScriptBin, ... }:

let
  defaultPython = pkgs.pixPkgs.python;
  python = defaultPython.override {
    userPipPrefix = ''''${XDG_DATA_HOME:-$HOME/.local/share}/${defaultPython.userPipPrefix}'';
  };

  initUserPyenvScript = writeScriptBin "init-user-pyenv.sh" ''
    DIR="''${USER_PYENV_DIR:-${python.userPipPrefix}}"

    if python -m venv --system-site-packages --copies --upgrade "$DIR"; then
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
      export USER_PYENV_DIR="${python.userPipPrefix}"
      export PYTHONPATH="${python.userPythonPath}:$PYTHONPATH"
      export PATH="${python.userPath}:$PATH"
      unset SOURCE_DATE_EPOCH
    '';
  };

in fhsenv.env
