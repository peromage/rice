{ config, pkgs, lib, ... }:

let
  cfg = config.pix.homeprogs.python;
  defaultPython = pkgs.pixPkgs.python;
  ## NOTE: Not passing a override function here since the `userPipPrefix' has
  ## a default value so prev.userPipPrefix will be missing during build.
  python = defaultPython.override {
    userPipPrefix = "${config.xdg.dataHome}/${defaultPython.userPipPrefix}";
  };

in with lib; {
  options.pix.homeprogs.python = {
    enable = mkEnableOption "Python3";
  };

  config = mkIf cfg.enable {
    home.sessionVariables = {
      PIP_PREFIX = python.userPipPrefix;
      PYTHONPATH = "${python.userPythonPath}:$PYTHONPATH";
    };
    home.sessionPath = [
      python.userPath
    ];
    home.packages = [ python ];
  };
}
