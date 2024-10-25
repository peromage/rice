{ config, lib, pkgs, ... }:

let
  cfg = config.pix.services.ime.fcitx;
  gnomeCfg = config.pix.desktops.env.gnome;

in with lib; {
  options.pix.services.ime.fcitx = {
    enable = mkEnableOption "Fcitx5";

    layout = mkOption {
      type = types.str;
      default = "us";
      description = "X keyboard layout, or multiple keyboard layouts separated by commas.";
    };
  };

  config = mkIf cfg.enable {
    services.xserver.xkb.layout = cfg.layout;

    i18n.inputMethod = {
      enable = true;
      type = "fcitx5";
      fcitx5.addons = with pkgs; [
        fcitx5-rime
        fcitx5-configtool
        fcitx5-chinese-addons
        fcitx5-gtk
      ];
    };

    environment.systemPackages = with pkgs; [
      librime
      rime-cli
      rime-data
    ]
    ++ optional gnomeCfg.enable gnomeExtensions.kimpanel;
  };
}
