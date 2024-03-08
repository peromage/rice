### Convenient top level imports

{ rice, ... }:

{
  ## NOTE: The subdirectories are not imported recursively.
  imports = rice.lib.listDirNoDefault ./.;

  ## System state version
  system.stateVersion = "24.05";
}
