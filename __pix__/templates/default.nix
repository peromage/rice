{ rice, ... }:

with rice.lib; importAllNameMapped
  baseNameNoExt
  (listDir isNotDefaultNix ./.)
