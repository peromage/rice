{ pkgs, ... }:

pkgs.python3.withPackages (pyPkgs: with pyPkgs; [
  pip
  pipx
  wheel
  # poetry-core # Can be installed via pipx
])
