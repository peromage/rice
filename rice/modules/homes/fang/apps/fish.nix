{ rice, ... }:

let
  src = "${rice.dirs.dotfiles}/fish";

in {
  programs.fish = {
    enable = true;
    shellInit = "";
    loginShellInit = "";
    interactiveShellInit = ''
source ${src}/.config/fish/config.fish
'';
  };
}
