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
  imports = rice.lib.allWithFilter
    (n: v: with builtins; "default.nix" != n && isNull (match "^DISABLED-.*" n))
    ./.;

  xdg.enable = true;

  home.file = {
    "bin" = {
      source = "${src}/bin/bin";
      recursive = true;
    };

    ".vim" = {
      source = "${src}/vim/.vim";
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
