{ config, lib, ... }:

let
  cfg = config.pix.services.vconsole;

in {
  options.pix.services.vconsole = {
    enable = lib.mkEnableOption "virtual console";
  };

  config = lib.mkIf cfg.enable {
    console = {
      enable = true;
      earlySetup = false;
      /* There is a bug where font is set other than default the null, for
         example, "Lat2-Terminus16", the `systemd-vconsole-setup.service' will
         fail to start in stage 1.
         Ref: https://github.com/NixOS/nixpkgs/issues/257904
      */
      font = null;
      useXkbConfig = true; # use xkbOptions in tty.
    };
  };
}
