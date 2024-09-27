{ pix, ... }:

{
  imports = with pix.lib; listDir isNotDefaultNix ./.;
}
