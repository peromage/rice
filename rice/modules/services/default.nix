### All service options

{ lib, rice, ... }:

let
  librice = rice.lib;

in with lib; {
  imports = librice.allButDefault ./.;

  options.rice.services = {
    ## SSH
    ssh = {
      enable = mkEnableOption "Enable SSH service.";
    };

    ## VPN: Globalprotect
    globalprotect = {
      enable = mkEnableOption "Enable GlobalProtect VPN client.";
    };
  };
}
