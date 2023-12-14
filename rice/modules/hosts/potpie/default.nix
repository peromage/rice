{ rice, lib, ... }:

{
  imports = rice.lib.allButDefault ./.;

  options.rice.hosts.profiles.potpie = with lib; {
    enable = mkEnableOption "Host activation";

    name = mkOption {
      type = types.str;
      default = "Potpie";
      description = "Host name";
    };

    disableFirewall = lib.mkEnableOption "Firewall";
  };
}
