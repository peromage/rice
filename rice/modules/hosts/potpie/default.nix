{ rice, ... }:

let
  librice = rice.lib;

in {
  imports = librice.allButDefault ./.;

  ## Default host name
  rice.hosts.hosts.potpie.name = "Potpie";

  ## System state version
  system.stateVersion = "23.11";
}
