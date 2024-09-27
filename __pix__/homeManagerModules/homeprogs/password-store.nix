{ config, lib, pkgs, ... }:

let
  cfg = config.pix.homeprogs.password-store;

in with lib; {
  options.pix.homeprogs.password-store = {
    enable = mkEnableOption "Password Store";
  };

  config = mkIf cfg.enable {
    programs.password-store = {
      enable = true;
      settings = {
        PASSWORD_STORE_DIR = "${config.home.homeDirectory}/.password-store";
        PASSWORD_STORE_CLIP_TIME = "30";
      };
      package = pkgs.pass.withExtensions (exts: with exts; [
        pass-otp
        pass-genphrase
      ]);
    };
  };
}
