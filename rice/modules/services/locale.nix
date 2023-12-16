{ config, lib, rice, ... }:

let
  librice = rice.lib;
  cfg = config.rice.services.locale;

in with lib; {
  options.rice.services.locale = {
    enabled = mkOption {
      type = with types; nullOr str;
      default = null;
      description = "Default locale settings";
    };

    timeZone = mkOption {
      type = with types; nullOr str;
      default = null;
      description = "Default time zone";
    }
  };

  config = librice.mkMergeIf [
    {
      cond = null != cfg.enabled;
      as = let lc = cfg.enabled; in {
        i18n = {
          defaultLocale = lc;
          extraLocaleSettings = {
            LC_ADDRESS = lc;
            LC_IDENTIFICATION = lc;
            LC_MEASUREMENT = lc;
            LC_MONETARY = lc;
            LC_NAME = lc;
            LC_NUMERIC = lc;
            LC_PAPER = lc;
            LC_TELEPHONE = lc;
            LC_TIME = lc;
          };
        };
      };
    }

    {
      cond = null != cfg.timeZone;
      as = {
        time.timeZone = cfg.timeZone;
      };
    }
  ];
}
