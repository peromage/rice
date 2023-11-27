{ config, lib, rice, ... }:

let
  inherit (rice) nixpkgs;
  inherit (rice.inputs) lanzaboote nixos-hardware;

in
{
  imports = [
    nixpkgs.nixosModules.notDetected
    lanzaboote.nixosModules.lanzaboote
    nixos-hardware.nixosModules.framework-12th-gen-intel
    ./mounts.nix
    ./packages.nix
  ];

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  boot = {
    initrd = {
      systemd.enable = true;
      luks.devices."ffroot".device = "/dev/disk/by-uuid/d698d7a5-125f-46ad-bc1d-47f9807afdef";

      availableKernelModules = [
        "xhci_pci"
        "thunderbolt"
        "nvme"
        "usb_storage"
        "sd_mod"
      ];

      kernelModules = [
        "tpm"
        "tpm_crb"
        "tpm_tis"
      ];
    };

    kernelModules = [
      "kvm-intel"
    ];

    kernelParams = [
      "nvme.noacpi=1" # Sleep power reduction
    ];

    extraModulePackages = [ ];

    # Use the systemd-boot EFI boot loader.
    # Secure boot: https://nixos.wiki/wiki/Secure_Boot
    bootspec.enable = true;

    lanzaboote = {
      enable = true;
      pkiBundle = "/etc/secureboot";
    };

    loader = {
      systemd-boot.enable = lib.mkForce false;
      efi.canTouchEfiVariables = true;
    };
  };

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
