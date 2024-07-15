{ config, lib, pix, ... }:

let
  libpix = pix.lib;

  cfg = config.pix.services.libvirtd;

  options = with lib; {
    enable = mkEnableOption "virtual manager";
    enableIntelSRIOV = mkEnableOption "Intel SR-IOV";
  };

in {
  options.pix.services.libvirtd = options;

  config = libpix.mkMergeIf [
    {
      cond = cfg.enable;
      as = {
        virtualisation.libvirtd = {
          enable = true;
          onBoot = "ignore";
          onShutdown = "suspend";
        };
        programs.virt-manager.enable = true;
      };
    }

    /* FIXME: Missing a DKMS module:
       https://github.com/strongtz/i915-sriov-dkms
    */
    {
      cond = cfg.enable && cfg.enableIntelSRIOV;
      as = {
        boot = {
          kernelParams = [
            "intel_iommu=on"
            "i915.enable_guc=3"
            "i915.max_vfs=7"
          ];

          kernelModules = [
            "i915"
          ];
        };
      };
    }
  ];
}
