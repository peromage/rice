{ pkgs, rice, ... }:

let
  src = "${rice.paths.dotfiles}/fcitx5";

in {
  i18n.inputMethod = {
    enabled = "fcitx5";
    fcitx5.addons = with pkgs; [
      fcitx5-rime
      fcitx5-configtool
      fcitx5-chinese-addons
      fcitx5-gtk
    ];
  };

  home.packages = with pkgs; [
    librime
    rime-cli
    rime-data
  ];

  xdg.configFile."fcitx5" = {
    source = "${src}/.config/fcitx5";
    recursive = true;
  };

  xdg.dataFile."fcitx5" = {
    source = "${src}/.local/share/fcitx5";
    recursive = true;
  };
}
