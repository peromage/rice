{ config, lib, ... }:

with lib;
let
  cfg = config.rice.powerGovernor;

in {
  options.rice.powerGovernor = {
    enable = mkEnableOption "Enable power governor";

    profile = mkOption {
      type = types.enum [ "ondemand" "powersave" "performance" ];
      default = "ondemand";
      description = "Default power governor profile";
    };
  };

  config = mkIf cfg.enable {
    powerManagement.cpuFreqGovernor = cfg.profile;
  };
}
