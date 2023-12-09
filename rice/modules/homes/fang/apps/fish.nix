{ rice, ... }:

let
  src = "${rice.dirs.dotfiles}/fish/.config/fish";

in {
  programs.fish = {
    enable = true;
    shellInit = "";
    loginShellInit = "";
    interactiveShellInit = ''
source ${src}/config.fish
'';
  };

  xdg.configFile = {
    "fish/functions" = {
      source = "${src}/functions";
      recursive = true;
    };
  };
}
