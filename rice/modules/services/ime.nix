{ config, pkgs, lib, librice, ... }:

let
  inherit (lib) types mkEnableOption mkOption optional;
  inherit (librice) mkMergeIf;

  cfg = config.rice.services.ime;

  options = {
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

  gnomeEnabled = config.rice.desktops.env.gnome.enable;

in {
  options.rice.services.ime = options;

  config = mkMergeIf [
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
        ++ optional gnomeEnabled gnomeExtensions.kimpanel;
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
