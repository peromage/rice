### Convenient top level imports

{ rice, ... }:

{
  ## NOTE: The subdirectories are not imported recursively.
  imports = rice.lib.allButDefault ./.;

  ## System state version
  system.stateVersion = "23.11";
}
