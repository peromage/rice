{ config, lib, pix, ... }:

let
  cfg = config.pix.homeprogs.wezterm;
  src = "${pix.path.dotfiles}/wezterm/.config/wezterm";

in with lib; {
  options.pix.homeprogs.wezterm = {
    enable = mkEnableOption "Wez's Terminal";
  };

  config = mkIf cfg.enable {
    programs.wezterm = {
      enable = true;
      # enableBashIntegration = true;
    };

    xdg.configFile."wezterm" = {
      source = src;
      recursive = true;
    };
  };
}
