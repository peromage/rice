/* Python runtime.

   The Python package is not meant to go in to system/user environment since
   Nix doesn't allow site package directory to be mutable.

   This package here is solely to provide runtime support for arbitrary script
   execution and simple project management.

   For regular Python tools, use `pipx' to install in an ad-hoc fashion, if they
   are not intended to be managed by Nix config.

   For dev dependency management, use `poetry'.

   For virtual environment, use `poetry shell'.

   For more complicated use case, use `nix-shell' or `nix develop'.
*/

{ pkgs, ... }:

let
  myPython = pkgs.python3.withPackages (pyPkgs: with pyPkgs; [
    ## Essential for building Python packages
    setuptools
    wheel
    poetry-core

    ## Package management
    pip
    pipx # Ad-hoc user space tool
  ]);

in pkgs.buildEnv {
  name = "my-python3";
  paths = [
    myPython
    pkgs.poetry
  ];
  passthru = {
    sitePackages = myPython.sitePackages;
  };
}
