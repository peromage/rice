{ config, lib, pkgs, ... }:

let
  cfg = config.pix.services.globalprotect;

in {
  options.pix.services.globalprotect = {
    enable = lib.mkEnableOption "GlobalProtect VPN client";
  };

  config = lib.mkIf cfg.enable {
    services.globalprotect.enable = true;
    environment.systemPackages = with pkgs; [
      globalprotect-openconnect
    ];
  };
}
