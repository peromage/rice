{ config, lib, pix, ... }:

let
  cfg = config.pix.homepkgs.vim;
  src = "${pix.path.dotfiles}/vim/.vim";

in with lib; {
  options.pix.homepkgs.vim = {
    enable = mkEnableOption "Vim";
  };

  config = mkIf cfg.enable {
    programs.vim.enable = true;

    home.file.".vim" = {
      source = src;
      recursive = true;
    };
  };
}
