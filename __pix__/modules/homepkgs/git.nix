{ config, lib, pix, ... }:

let
  cfg = config.pix.homepkgs.git;
  src = "${pix.path.dotfiles}/git/.config/git";

in with lib; {
  options.pix.homepkgs.git = {
    enable = mkEnableOption "Git";

    extraIncludes = mkOption {
      type = with types; listOf attrs;
      default = [];
      description = ''
        Extra inlcudes of git config.
        This is equivalent to `programs.git.includes';
      '';
    };
  };

  config = mkIf cfg.enable {
    programs.git = {
      enable = true;
      lfs.enable = true;
      includes = [
        { path = "${src}/config"; }
        { path = "${src}/user-fang"; }
      ];
    };
  };
}