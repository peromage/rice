{ pix, ... }:

with pix.lib; importAllNameMapped
  baseNameNoExt
  (listDir isNotDefaultNix ./.)
