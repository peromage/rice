### All service options

{ librice, ... }:

let
  inherit (librice) filterDir isNotDefaultNix;

in {
  imports = filterDir isNotDefaultNix ./.;
}
