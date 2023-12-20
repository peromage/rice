/* Information is from https://nixos.wiki/wiki/Secure_Boot

   Simplified steps:
   1. Run: sudo nix run nixpkgs#sbctl create-keys
   2. Import this module, build and reboot.
   3. Run: sudo nix run nixpkgs#sbctl verify
   4. Reboot and set UEFI in setup mode.
   5. Run: sudo nix run nixpkgs#sbctl enroll-keys -- --microsoft
   6. Run: bootctl status

   NOTE: To avoid bringing in unnecessary dependencies this service module does
   not import `lanzaboote' by itself.  The caller who wishes to enable this
   module needs to import it explicitly.

   For example:
   imports = [ lanzaboote.nixosModules.lanzaboote ];
*/

{ config, pkgs, lib, rice, ... }:

let
  cfg = config.rice.services.secureboot;

  options  = with lib; {
    enable = mkEnableOption "secure boot support";
  };

in {
  options.rice.services.secureboot =  options;

  config = with lib; mkIf cfg.enable {
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
