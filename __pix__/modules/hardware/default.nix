### All service options

{ pix, ... }:

{
  imports = with pix.lib; listDir isNotDefaultNix ./.;
}
