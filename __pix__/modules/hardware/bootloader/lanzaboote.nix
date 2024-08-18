{ config, lib, pkgs, lanzaboote, ... }:

let
  cfg = config.pix.hardware.bootloader.lanzaboote;

in with lib; {
  imports = [ lanzaboote.nixosModules.lanzaboote ];

  options.pix.hardware.bootloader.lanzaboote = {
    enable = mkEnableOption ''
      Lanzaboote bootloader for secureboot

      This option implies systemd-boot.

      Note: When installing the system for the first time, `systemd-boot' or
      `grub' should be used and switch to `lanzaboote' after.  The steps are
      as follow:

      For a clean installation:
      1. Disable secure boot in BIOS.
      2. In NixOS: sudo nix run nixpkgs#sbctl create-keys
      3. Switch to the option `lanzaboote', build and reboot.
      4. In NixOS: sudo nix run nixpkgs#sbctl -- verify && sudo nix run nixpkgs#sbctl -- status
      5. Reboot and set BIOS in setup mode (Reset all secure boot keys and secure boot should be disabled).
      6. In NixOS: sudo nix run nixpkgs#sbctl enroll-keys -- --microsoft
      7. Reboot and enable secure boot in BIOS.
      8. In NixOS: bootctl status

      To regain secure boot on an existing installation (with Lanzaboote already), start from step 5.
    '';
  };

  config = mkIf cfg.enable {
    boot = {
      bootspec.enable = true;
      lanzaboote = {
        enable = true;
        pkiBundle = "/etc/secureboot";
      };

      loader = {
        grub.enable = mkForce false;
        systemd-boot.enable = mkForce false;
        efi.canTouchEfiVariables = false;
      };
    };

    environment.systemPackages = with pkgs; [
      sbctl
    ];
  };
}
