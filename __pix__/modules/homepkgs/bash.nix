{ config, lib, pix, ... }:

let
  cfg = config.pix.homepkgs.bash;
  src = "${pix.path.dotfiles}/bash";

in with lib; {
  options.pix.homepkgs.bash = {
    enable = mkEnableOption "Bash";
  };

  config = mkIf cfg.enable {
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
  };
}
