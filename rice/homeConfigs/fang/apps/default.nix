### Reusing existing dotfiles

/* Mapped paths from Home Manager's variables:

   `~/.config': config.xdg.configHome
   `~/.local/share': config.xdg.dataHome

   Shorthands for creating files under directories:

   `~': home.file.<name>
   Ref: https://nix-community.github.io/home-manager/options.html#opt-home.file

   `~/.config': xdg.configFile.<name>
   Ref: https://nix-community.github.io/home-manager/options.html#opt-xdg.configFile

   `~/.local/share': xdg.dataFile.<name>
   Ref: https://nix-community.github.io/home-manager/options.html#opt-xdg.dataFile
*/

{ rice, ... }:

let
  src = rice.paths.dotfiles;

in {
  imports = with rice.lib; listDir (n: t: isNotDefaultNix n t && isNotDisabled n t) ./.;

  xdg.enable = true;

  home.file = {
    "bin" = {
      source = "${src}/bin/bin";
      recursive = true;
    };
  };

  xdg.configFile = {
    "powershell" = {
      source = "${src}/pwsh/.config/powershell";
      recursive = true;
    };

    "wezterm" = {
      source = "${src}/wezterm/.config/wezterm";
      recursive = true;
    };
  };

  ## Comment out this if `fcitx5.nix' is enabled
  xdg.configFile."fcitx5" = {
    source = "${src}/fcitx5/.config/fcitx5";
    recursive = true;
  };
  xdg.dataFile."fcitx5" = {
    source = "${src}/fcitx5/.local/share/fcitx5";
    recursive = true;
  };
}
