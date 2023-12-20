{ config, lib, rice, ... }:

let
  inherit (lib) types mkEnableOption mkOption mkIf;

  cfg = config.rice.services.i18n;

  options = {
    enable = mkEnableOption "internationalization";

    locale = mkOption {
      type = types.str;
      default = "en_US.UTF-8";
      description = "Default locale settings.";
    };

    timeZone = mkOption {
      type = types.str;
      default = "America/Detroit";
      description = "Default time zone.";
    };
  };

  lc = cfg.locale;

in {
  options.rice.services.i18n = options;

  config = mkIf cfg.enable {
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
    time.timeZone = cfg.timeZone;
  };
}
