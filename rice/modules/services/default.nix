### All service options

{ rice, ... }:

let
  inherit (rice.lib) filterDir isNotDefaultNix;

in {
  imports = filterDir isNotDefaultNix ./.;
}
