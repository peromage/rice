{ config, lib, ... }:

let
  inherit (lib) types mkEnableOption mkOption mkIf;

  cfg = config.rice.services.power;

  options = {
    enable = mkEnableOption "power governor";

    profile = mkOption {
      type = types.enum [ "ondemand" "powersave" "performance" ];
      default = "ondemand";
      description = "Default power governor profile.";
    };
  };

in {
  options.rice.services.power = options;

  config = mkIf cfg.enable {
    powerManagement.cpuFreqGovernor = cfg.profile;
  };
}
