### Convenient top level imports

{ rice, ... }:

let
  inherit (rice.lib) filterDir isNotDefaultNix;

in {
  ## NOTE: The subdirectories are not imported recursively.
  imports = filterDir isNotDefaultNix ./.;

  ## System state version
  system.stateVersion = "24.05";
}
