### Sucure boot support

/* Information is from https://nixos.wiki/wiki/Secure_Boot

   Simplified steps:
   1. Run: sudo nix run nixpkgs#sbctl create-keys
   2. Import this module, build and reboot.
   3. Run: sudo nix run nixpkgs#sbctl verify
   4. Reboot and set UEFI in setup mode.
   5. Run: sudo nix run nixpkgs#sbctl enroll-keys -- --microsoft
   6. Run: bootctl status
*/

{ config, pkgs, lib, rice, ... }:

let
  inherit (rice.flake.inputs) lanzaboote;
  cfg = config.rice.hardware.secureBoot;

in with lib; {
  imports = [
    lanzaboote.nixosModules.lanzaboote
  ];

  options.rice.hardware.secureBoot = {
    enable = mkEnableOption "Secure boot support";
  };

  config = mkIf cfg.enable {
    boot = {
      bootspec.enable = true;

      lanzaboote = {
        enable = true;
        pkiBundle = "/etc/secureboot";
      };

      loader = {
        systemd-boot.enable = lib.mkForce false;
        efi.canTouchEfiVariables = false;
      };
    };

    environment.systemPackages = with pkgs; [
      sbctl
    ];
  };
}
