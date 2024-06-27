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
}
