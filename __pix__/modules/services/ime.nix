{ config, pkgs, lib, pix, ... }:

let
  libpix = pix.lib;

  cfg = config.pix.services.ime;

  options = with lib; {
    enabled = mkOption {
      type = with types; nullOr (enum [ "fcitx" "ibus" ]);
      default = null;
      description = "Enabled input method.";
    };

    layout = mkOption {
      type = types.str;
      default = "us";
      description = "X keyboard layout, or multiple keyboard layouts separated by commas.";
    };
  };

  gnomeEnabled = config.pix.desktops.env.gnome.enable;

in {
  options.pix.services.ime = options;

  config = libpix.mkMergeIf [
    {
      cond = "ibus" == cfg.enabled;
      as = {
        services.xserver.xkb.layout = cfg.layout;

        i18n.inputMethod = {
          enabled = "ibus";
          ibus.engines = with pkgs.ibus-engines; [
            rime
            libpinyin
          ];
        };

        environment.systemPackages = with pkgs; [
          librime
          rime-cli
          rime-data
        ];
      };
    }

    {
      cond = "fcitx" == cfg.enabled;
      as = {
        services.xserver.xkb.layout = cfg.layout;

        i18n.inputMethod = {
          enabled = "fcitx5";
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
        ++ lib.optional gnomeEnabled gnomeExtensions.kimpanel;
      };
    }
  ];
}
