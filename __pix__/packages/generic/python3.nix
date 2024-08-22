{ pkgs, ... }:

pkgs.python3.withPackages (pyPkgs: with pyPkgs; [
  pip
])
