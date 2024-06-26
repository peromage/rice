{ lib, ... }:

lib.mkMerge [
  /* Homebrew basic config

     Note that Homebrew will not be installed automatically even `homebrew.enable'
     is turned on.  It needs manual installation.

     Installation command:
       /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

     See: https://daiderd.com/nix-darwin/manual/index.html#opt-homebrew.enable
  */
  {
    homebrew.enable = true;
  }

  /* Limit battery charge.

     Enable with the following commands:
       sudo bclm write 75
       sudo bclm persist
  */
  {
    homebrew.taps = [ "zackelia/formulae" ];
    homebrew.brews = [ "bclm" ];
  }
]
