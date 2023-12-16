{ config, pkgs, lib, rice, ... }:

let
  librice = rice.lib;
  cfg = config.rice.services.ime;
  gnomeEnabled = config.rice.desktops.env.gnome.enable;

in with lib; {
  options.rice.services.ime = {
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

  config = librice.mkMergeIf [
    {
      cond = "ibus" == cfg.enabled;
      as = {
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

    {
      cond = null != cfg.enabled;
      as = {
        services.xserver.xkb.layout = cfg.layout;
      };
    }
  ];
}
