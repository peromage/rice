### Convenient top level imports

{ rice, ... }:

let
  librice = rice.lib;

in {
  ## NOTE: The subdirectories are not imported recursively.
  imports = librice.allButDefault ./.;
}
