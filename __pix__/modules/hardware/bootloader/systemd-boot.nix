{ config, lib, ... }:

let
  cfg = config.pix.hardware.bootloader.systemd-boot;

in with lib; {
  options.pix.hardware.bootloader.systemd-boot = {
    enable = mkEnableOption "Systemd-boot bootloader";
  };

  config = mkIf cfg.enable {
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
