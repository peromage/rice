{ lib, ... }:

{
  ## Disk
  fileSystems."/" =
    { device = "/dev/disk/by-uuid/9213f352-4bf8-4e57-824a-7135aaee46bf";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/43E4-9AB3";
      fsType = "vfat";
    };

  swapDevices = [ ];

  ## Bootloader.
  boot.initrd.availableKernelModules = [ "ahci" "xhci_pci" "usbhid" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
