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
  inherit (lib) mkEnableOption mkIf mkForce;
  inherit (rice.flake.inputs) lanzaboote;

  cfg = config.rice.services.secureboot;

  options = {
    enable = mkEnableOption "secure boot support";
  };

in {
  imports = [ lanzaboote.nixosModules.lanzaboote ];
  options.rice.services.secureboot = options;

  config = mkIf cfg.enable {
    boot = {
      bootspec.enable = true;

      lanzaboote = {
        enable = true;
        pkiBundle = "/etc/secureboot";
      };

      loader = {
        systemd-boot.enable = mkForce false;
        efi.canTouchEfiVariables = false;
      };
    };

    environment.systemPackages = with pkgs; [
      sbctl
    ];
  };
}
