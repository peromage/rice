### All service options

{ rice, ... }:

{
  imports = with rice.lib; listDir isNotDefaultNix ./.;
}
