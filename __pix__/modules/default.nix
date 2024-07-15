### Convenient top level imports

{ pix, ... }:

{
  ## NOTE: The subdirectories are not imported recursively.
  imports = with pix.lib; listDir isNotDefaultNix ./.;

  ## System state version
  system.stateVersion = "24.05";
}
