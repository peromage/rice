{ lib, ... }:

let
  withDefaultSubvolOptions =
    { vol
    , compress ? true
    , cow ? true
    , extraOptions ? []
    }: {
      device = "/dev/disk/by-uuid/35154f6e-27aa-49f8-b1b6-6472127cb524";
      fsType = "btrfs";
      options = [
        "subvol=${vol}"
        "ssd"
        "noatime"
        "autodefrag"
      ]
      ++ (lib.optional compress "compress=zstd:3")
      ++ (lib.optional (!cow) "nodatacow")
      ++ extraOptions;
    };

in {
  fileSystems."/" = withDefaultSubvolOptions { vol = "@nixos"; };
  fileSystems."/nix" = withDefaultSubvolOptions { vol = "@nixstore"; };
  fileSystems."/home" = withDefaultSubvolOptions { vol = "@home"; };

  fileSystems."/ff/swap" = withDefaultSubvolOptions {
    vol = "@swap";
    compress = false;
    cow = false;
  };

  fileSystems."/ff/vm" = withDefaultSubvolOptions {
    vol = "@vm";
    compress = false;
    cow = false;
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/8F86-D998";
    fsType = "vfat";
  };

  swapDevices = [
    {
      device = "/ffstore/swap/swap32gb.img";
    }
  ];
}
