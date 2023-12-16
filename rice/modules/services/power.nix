{ config, lib, ... }:

let
  cfg = config.rice.services.powerGovernor;

in with lib; {
  options.rice.services.powerGovernor = {
    enable = mkEnableOption "power governor";

    profile = mkOption {
      type = types.enum [ "ondemand" "powersave" "performance" ];
      default = "ondemand";
      description = "Default power governor profile.";
    };
  };

  config = mkIf cfg.enable {
    powerManagement.cpuFreqGovernor = cfg.profile;
  };
}
