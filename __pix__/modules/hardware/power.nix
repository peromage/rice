{ config, lib, ... }:

let
  cfg = config.pix.hardware.power;

in {
  options.pix.hardware.power = with lib; {
    enable = mkEnableOption "power governor";

    profile = mkOption {
      type = types.enum [ "ondemand" "powersave" "performance" ];
      default = "ondemand";
      description = "Default power governor profile.";
    };
  };

  config = lib.mkIf cfg.enable {
    powerManagement.cpuFreqGovernor = cfg.profile;
  };
}
