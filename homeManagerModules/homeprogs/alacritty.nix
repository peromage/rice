{ config, lib, pix, ... }:

let
  cfg = config.pix.homeprogs.alacritty;
  src = "${pix.path.dotfiles}/alacritty/.config/alacritty";

in with lib; {
  options.pix.homeprogs.alacritty = {
    enable = mkEnableOption "Alacritty";
  };

  config = mkIf cfg.enable {
    programs.alacritty.enable = true;

    xdg.configFile."alacritty" = {
      source = src;
      recursive = true;
    };
  };
}
