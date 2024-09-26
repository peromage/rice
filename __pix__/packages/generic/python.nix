/* Python runtime.

   This package here is solely to provide runtime support for arbitrary script
   execution and simple project management.

   The Python package is not meant to go in to system/user environment since
   Nix doesn't allow site package directory to be mutable.  To circumvent this
   limitation, there are three options:

   1. Create a virtual environment by `python -m venv' then install packages in
      it.  This works for most of the times but might be inconvenient sometimes
      since it requires activating the virtual environment every time.

   2. Set environment variables for pip and python so that packages can be installed
      and searched "globally".
      a. `PIP_PREFIX': The location where pip packages are installed.
      b. `PYTHONPATH': Where to search for installed packages. E.g.
         PYTHONPATH="$PIP_PREFIX/${pkgs.python3.sitePackages}:$PYTHONPATH"
      c. `PATH': Where to search for installed utilities from packages. E.g.
         PATH="$PIP_PREFIX/bin:$PATH"

   3. Use `pipx' to install executable utilities but libraries are not supported
      due to the isolation nature.

   For dev dependency management, use `poetry'.  Also, poetry can be use as a
   virtual environment by entering the shell with `poetry shell'.

   For more complicated use cases, for example, some package installations require
   compiling C code, use `nix-shell' or `nix develop' in a standard FHS environment.
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
