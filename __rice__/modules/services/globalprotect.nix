{ config, lib, pkgs, ... }:

let
  cfg = config.rice.services.globalprotect;

  options = {
    enable = lib.mkEnableOption "GlobalProtect VPN client";
  };

in {
  options.rice.services.globalprotect = options;

  config = lib.mkIf cfg.enable {
    services.globalprotect.enable = true;
    environment.systemPackages = with pkgs; [
      globalprotect-openconnect
    ];
  };
}
