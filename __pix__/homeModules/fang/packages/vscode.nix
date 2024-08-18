{ config, lib, pix, ... }:

let
  cfg = config.pix.homepkgs.vscode;
  src = "${pix.path.dotfiles}/vscode/.config/Code";

in with lib; {
  options.pix.homepkgs.vscode = {
    enable = mkEnableOption "Visual Studio Code";
  };

  config = mkIf cfg.enable {
    programs.vscode.enable = true;

    xdg.configFile."Code" = {
      source = src;
      recursive = true;
    };
  };
}
