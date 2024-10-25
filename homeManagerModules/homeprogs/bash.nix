{ config, lib, pix, ... }:

let
  cfg = config.pix.homeprogs.bash;
  src = "${pix.path.dotfiles}/bash";

in with lib; {
  options.pix.homeprogs.bash = {
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
  };
}
