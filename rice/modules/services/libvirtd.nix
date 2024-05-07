{ config, lib, librice, ... }:

let
  inherit (lib) mkEnableOption mkIf;
  inherit (librice) mkMergeIf;

  cfg = config.rice.services.libvirtd;

  options = {
    enable = mkEnableOption "virtual manager";
    enableIntelSRIOV = mkEnableOption "Intel SR-IOV";
  };

in {
  options.rice.services.libvirtd = options;

  config = mkMergeIf [
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
