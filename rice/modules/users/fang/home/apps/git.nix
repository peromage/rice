{ rice, ... }:

let
  src = "${rice.dirs.dotfiles}/git/.config/git";

in {
  programs.git = {
    enable = true;
    lfs.enable = true;
    includes = [
      { path = "${src}/config"; }
      { path = "${src}/user-fang"; }
    ];
  };
}
