{ config, pkgs, lib, ... }:

let
  cfg = config.pix.services.ime;
  gnomeEnabled = config.pix.desktops.env.gnome.enable;

in {
  options.pix.services.ime = with lib; {
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

  config = with lib; mkMerge [
    (mkIf ("ibus" == cfg.enabled) {
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
    })

    (mkIf ("fcitx" == cfg.enabled) {
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
    })
  ];
}
