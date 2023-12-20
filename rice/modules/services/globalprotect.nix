{ config, lib, pkgs, ... }:

let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.rice.services.globalprotect;

  options = {
    enable = mkEnableOption "GlobalProtect VPN client";
  };

in {
  options.rice.services.globalprotect = options;

  config = mkIf cfg.enable {
    services.globalprotect.enable = true;
    environment.systemPackages = with pkgs; [
      globalprotect-openconnect
    ];
  };
}
