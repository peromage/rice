{ config, lib, ... }:

let
  cfg = config.rice.hardware.powerGovernor;

in with lib; {
  options.rice.hardware.powerGovernor = {
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
