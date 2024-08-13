{ config, pkgs, lib, pix, nixos-hardware, nixpkgs, ... }:

{
  imports = [
    nixpkgs.nixosModules.notDetected
    nixos-hardware.nixosModules.framework-12th-gen-intel
  ];

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
  };

  environment.systemPackages = with pkgs; [
    tpm2-tss
  ];

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  pix.hardware = {
    boot.enabled = "secure-boot";
    networking.enable = true;
    bluetooth.enable = true;
    firmware.enable = true;
    peripherals.enable = true;
    audio.enable = true;
    power = {
      enable = true;
      profile = "powersave";
    };
  };
}
