### Convenient top level imports

{ rice, ... }:

{
  ## NOTE: The subdirectories are not imported recursively.
  imports = with rice.lib; listDir isNotDefaultNix ./.;

  ## System state version
  system.stateVersion = "24.05";
}
