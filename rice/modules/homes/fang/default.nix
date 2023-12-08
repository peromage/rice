{ rice, ... }:

let
  librice = rice.lib;

in {
  imports = librice.allButDefault ./.;
}
