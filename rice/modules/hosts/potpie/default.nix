{ rice, ... }:

let
  librice = rice.lib;

in {
  imports = librice.allButDefault ./.;

  rice.hosts.hosts.potpie.name = "Potpie";
}
