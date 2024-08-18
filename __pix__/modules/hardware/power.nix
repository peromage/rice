{ config, lib, ... }:

let
  cfg = config.pix.hardware.power;

in with lib; {
  options.pix.hardware.power = {
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
