{ config, lib, ... }:

with lib;
let
  cfg = config.rice.power;

in {
  options.rice.power = {
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
