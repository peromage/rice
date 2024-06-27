{ rice, ... }:

let
  src = "${rice.paths.dotfiles}/wezterm/.config/wezterm";

in {
  programs.wezterm = {
    enable = true;
    enableBashIntegration = true;
  };

  xdg.configFile."wezterm" = {
    source = src;
    recursive = true;
  };
}
