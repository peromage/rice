### Convenient top level imports

{ rice, ... }:

let
  librice = rice.lib;

in {
  ## NOTE: The subdirectories are not imported recursively.
  imports = listDir isNotDefaultNix ./.;

  ## System state version
  system.stateVersion = "24.05";
}
