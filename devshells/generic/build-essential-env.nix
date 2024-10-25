/* An FHS environment with essential C/C++ utilities.

   This is to address an issue where traditional search path is required when
   building projects out of NixOS config.

   The `build-essential' package is adviced to be installed in the system to avoid
   being cleaned up from Nix store since it may contain runtime dependencies if
   somehow they are linked by their real path in Nix store.  However, in most
   cases, programs that requires FHS environment may still need to run within
   this shell.

   For example, the Python package `pynput' requires canonical headers from
   `linuxHeaders' package.
*/
{ pkgs, ... }:

let
  fhsenv = pkgs.buildFHSEnv {
    name = "c-fhs-env";
    targetPkgs = pkgs: [ pkgs.pixPkgs.build-essential ];
  };

in fhsenv.env
