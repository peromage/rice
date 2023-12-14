{ mkProfileOptions, ... }:
{ rice, lib, ... }:

{
  imports = rice.lib.allButDefault ./.;

  options.rice.hosts.profiles.potpie = mkProfileOptions {
    name = "Potpie";
  } // (with lib; {
    disableFirewall = mkEnableOption "Firewall";
  });
}
