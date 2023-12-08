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
  src = rice.dirs.dotfiles;

in {
  imports = rice.lib.allButDefault ./.;

  home.file = {
    ".vim" = {
      source = "${src}/vim/.vim";
      recursive = true;
    };

    ".gnupg" = {
      source = "${src}/gnupg/.gnupg";
      recursive = true;
    };
  };

  xdg.configFile = {
    "tmux" = {
      source = "${src}/tmux/.config/tmux";
      recursive = true;
    };

    "alacritty" = {
      source = "${src}/alacritty/.config/alacritty";
      recursive = true;
    };

    "Code" = {
      source = "${src}/vscode/.config/Code";
      recursive = true;
    };

    "mc" = {
      source = "${src}/mc/.config/mc";
      recursive = true;
    };

    "powershell" = {
      source = "${src}/pwsh/.config/powershell";
      recursive = true;
    };

    "wezterm" = {
      source = "${src}/wezterm/.config/wezterm";
      recursive = true;
    };
  };
}
