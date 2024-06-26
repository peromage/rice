{ rice, ... }:

let
  src = "${rice.paths.dotfiles}/bash";

in {
  programs.bash = {
    enable = true;
    enableCompletion = true;
    enableVteIntegration = true;
    bashrcExtra = "";
    profileExtra = "";
    logoutExtra = "";
    initExtra = ''
      source ${src}/.bashrc noenv
    '';
  };

  home.file = {
    ".librice" = {
      source = "${src}/.librice";
      recursive = true;
    };
  };
}
