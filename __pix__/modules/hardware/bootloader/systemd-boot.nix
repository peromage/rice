{ config, lib, ... }:

let
  cfg = config.pix.hardware.bootloader.systemd-boot;

in {
  options.pix.hardware.bootloader.systemd-boot = with lib; {
    enable = mkEnableOption "Systemd-boot bootloader";
  };

  config = with lib; mkIf cfg.enable {
    boot = {
      bootspec.enable = true;
      loader = {
        grub.enable = mkForce false;
        systemd-boot.enable = mkForce true;
        efi.canTouchEfiVariables = false;
      };
    };
  };
}
