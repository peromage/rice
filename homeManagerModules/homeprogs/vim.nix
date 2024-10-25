{ config, lib, pix, ... }:

let
  cfg = config.pix.homeprogs.vim;
  src = "${pix.path.dotfiles}/vim/.vim";

in with lib; {
  options.pix.homeprogs.vim = {
    enable = mkEnableOption "Vim";
  };

  config = mkIf cfg.enable {
    programs.vim = {
      enable = true;
      extraConfig = ''
        source ${src}/vimrc
      '';
    };
  };
}
