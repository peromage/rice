/* Python FHS environment.

   This is used as a "virtual" environment which allows to install user packages.
   This also supports some packages that require C compilation during installation.
*/

{ pkgs, ... }:

let
  fhsenv = pkgs.buildFHSEnv {
    name = "python-fhs-env";
    targetPkgs = pkgs: with pkgs; [
      pixPkgs.python
      pixPkgs.build-essential
    ];

    ## See: https://nixos.wiki/wiki/Python#Emulating_virtualenv_with_nix-shell
    profile = let p = pkgs.pixPkgs.python; in ''
      ## Enables pip install
      export PIP_PREFIX="$HOME/.local/share/${p.userPipPrefix}"
      export PYTHONPATH="$PIP_PREFIX/${p.sitePackages}:$PYTHONPATH"
      export PATH="$PIP_PREFIX/bin:$PATH"
      unset SOURCE_DATE_EPOCH
    '';
  };

in fhsenv.env
