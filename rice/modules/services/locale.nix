{ config, lib, rice, ... }:

let
  cfg = config.rice.services.locale;

  options = with lib; {
    enabled = mkOption {
      type = with types; nullOr str;
      default = null;
      description = "Default locale settings";
    };

    timeZone = mkOption {
      type = with types; nullOr str;
      default = null;
      description = "Default time zone";
    };
  };

  librice = rice.lib;

in {
  options.rice.services.locale = options;

  config = with lib; librice.mkMergeIf [
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
