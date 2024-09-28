{ config, pkgs, lib, ... }:

let
  cfg = config.pix.homeprogs.python;
  p = pkgs.pixPkgs.python;
  python = p.override {
    userPyenvDir = "${config.xdg.dataHome}/${p.userPyenvDir}";
  };

in with lib; {
  options.pix.homeprogs.python = {
    enable = mkEnableOption "Python3";
  };

  config = mkIf cfg.enable {
    home.sessionVariables = {
      PIP_PREFIX = python.userPyenvDir;
      PYTHONPATH = "${python.userPythonPath}:$PYTHONPATH";
    };
    home.sessionPath = [
      python.userPath
    ];
    home.packages = [ python ];
  };
}
