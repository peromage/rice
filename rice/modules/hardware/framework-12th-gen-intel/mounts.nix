{
  fileSystems."/" =
    { device = "/dev/disk/by-uuid/35154f6e-27aa-49f8-b1b6-6472127cb524";
      fsType = "btrfs";
      options = [ "subvol=@nixos" "ssd" "noatime" "compress=zstd:3" ];
    };

  fileSystems."/nix" =
    { device = "/dev/disk/by-uuid/35154f6e-27aa-49f8-b1b6-6472127cb524";
      fsType = "btrfs";
      options = [ "subvol=@nixstore" "ssd" "noatime" "compress=zstd:3" ];
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/35154f6e-27aa-49f8-b1b6-6472127cb524";
      fsType = "btrfs";
      options = [ "subvol=@home" "ssd" "noatime" "compress=zstd:3" ];
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/8F86-D998";
      fsType = "vfat";
    };

  fileSystems."/ffstore/swap" =
    { device = "/dev/disk/by-uuid/35154f6e-27aa-49f8-b1b6-6472127cb524";
      fsType = "btrfs";
      options = [ "subvol=@swap" "ssd" "noatime" "compress=zstd:3" ];
    };

  fileSystems."/ffstore/vm" =
    { device = "/dev/disk/by-uuid/35154f6e-27aa-49f8-b1b6-6472127cb524";
      fsType = "btrfs";
      options = [ "subvol=@vm" "ssd" "noatime" "compress=zstd:3" ];
    };

  swapDevices = [
    {
      device = "/ffstore/swap/swap32gb.img";
    }
  ];
}
