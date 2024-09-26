{ pkgs, ... }:

let
python = pkgs.python313;

in python.withPackages (pyPkgs: with pyPkgs; [
  pip
  pipx
  wheel
  # poetry-core # Can be installed via pipx
])
