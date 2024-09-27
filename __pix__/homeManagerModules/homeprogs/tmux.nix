{ config, lib, pix, ... }:

let
  cfg = config.pix.homeprogs.tmux;
  src = "${pix.path.dotfiles}/tmux/.config/tmux";

in with lib; {
  options.pix.homeprogs.tmux = {
    enable = mkEnableOption "Tmux";
  };

  config = mkIf cfg.enable {
    programs.tmux.enable = true;

    xdg.configFile."tmux" = {
      source = src;
      recursive = true;
    };
  };
}
