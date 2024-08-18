{ config, lib, ... }:

let
  cfg = config.pix.hardware.bootloader.grub;

in {
  options.pix.hardware.bootloader.grub = with lib; {
    enable = mkEnableOption "Grub bootloader";

    device = mkOption {
      type = types.str;
      default = "";
      description = "Same option `boot.loader.grub.device'.";
    };

    probeOS = mkEnableOption "Detect other OS";
  };

  config = with lib; mkIf cfg.enable {
    boot = {
      bootspec.enable = true;
      loader = {
        grub = {
          enable = mkForce true;
          efiSupport = true;
          device = cfg.grubDevice;
          useOSProber = cfg.probeOS;
        };
        systemd-boot.enable = mkForce false;
        efi.canTouchEfiVariables = false;
      };
    };
  };
}
