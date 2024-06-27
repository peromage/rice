{ rice, ... }:

let
  src = "${rice.paths.dotfiles}/vim/.vim";

in {
  programs.vim.enable = true;

  home.file.".vim" = {
    source = src
    recursive = true;
  };
}
