{ rice, ... }:

let
  inherit (rice.lib) mkPackageList;

in mkPackageList ./.;
