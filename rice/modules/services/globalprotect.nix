{ config, lib, pkgs, ... }:

let
  cfg = config.rice.services.globalprotect;

in with lib; {
  options.rice.services.globalprotect = {
    enable = mkEnableOption "GlobalProtect VPN client";
  };

  config = mkIf cfg.enable {
    services.globalprotect.enable = true;
    environment.systemPackages = with pkgs; [
      globalprotect-openconnect
    ];
  };
}
