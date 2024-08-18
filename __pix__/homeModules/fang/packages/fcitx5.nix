{ config, lib, pkgs, pix, ... }:

let
  cfg = config.pix.homepkgs.fcitx5;
  src = "${pix.path.dotfiles}/fcitx5";

in with lib; {
  options.pix.homepkgs.fcitx5 = {
    enable = mkEnableOption "Fcitx5";
  };

  config = mkIf cfg.enable {
    /* Temporarily disabled since this is problematic in user space.  Instead, the
     IME module should be enabled in NixOS config.
    */
    # i18n.inputMethod = {
    #   enabled = "fcitx5";
    #   fcitx5.addons = with pkgs; [
    #     fcitx5-rime
    #     fcitx5-configtool
    #     fcitx5-chinese-addons
    #     fcitx5-gtk
    #   ];
    # };

    # home.packages = with pkgs; [
    #   librime
    #   rime-cli
    #   rime-data
    # ];

    xdg.configFile."fcitx5" = {
      source = "${src}/.config/fcitx5";
      recursive = true;
    };

    xdg.dataFile."fcitx5" = {
      source = "${src}/.local/share/fcitx5";
      recursive = true;
    };
  };
}
