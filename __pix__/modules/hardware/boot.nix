{ config, pix, pkgs, lib, lanzaboote, ... }:

let
  libpix = pix.lib;

  cfg = config.pix.hardware.boot;

  options = with lib; {
    enabled = mkOption {
      type = with types; nullOr (enum [ "systemd-boot" "grub" "secure-boot"]);
      default = "systemd-boot";
      description = ''Default boot loader mode.

      If this is set to null no boot loader will be configured.

      The option `secure-boot' option implemented by `lanzaboote' which implies
      systemd-boot.

      Note: When installing the system for the first time, `systemd-boot' or
      `grub' should be used and switch to `secure-boot' after.  The steps are
      as follow:

      For a clean installation:
      1. Disable secure boot in BIOS.
      2. In NixOS: sudo nix run nixpkgs#sbctl create-keys
      3. Switch to the option `secure-boot', build and reboot.
      4. In NixOS: sudo nix run nixpkgs#sbctl -- verify && sudo nix run nixpkgs#sbctl -- status
      5. Reboot and set BIOS in setup mode (Reset all secure boot keys and secure boot should be disabled).
      6. In NixOS: sudo nix run nixpkgs#sbctl enroll-keys -- --microsoft
      7. Reboot and enable secure boot in BIOS.
      8. In NixOS: bootctl status

      To regain secure boot on an existing installation (with Lanzaboote already), start from step 5.
      '';
    };

    grubDevice = with lib; mkOption {
      type = types.str;
      default = "";
      description = "Same option `boot.loader.grub.device'.";
    };
  };

in {
  imports = [ lanzaboote.nixosModules.lanzaboote ];
  options.pix.hardware.boot = options;

  config = with lib; libpix.mkMergeIf [
    {
      cond = "systemd-boot" == cfg.enabled;
      as = {
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

    {
      cond = "grub" == cfg.enabled;
      as = {
        boot = {
          bootspec.enable = true;
          loader = {
            grub = {
              enable = mkForce true;
              device = cfg.grubDevice;
            };
            systemd-boot.enable = mkForce false;
            efi.canTouchEfiVariables = false;
          };
        };
      };
    }

    {
      cond = "secure-boot" == cfg.enabled;
      as = {
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
  ];
}
