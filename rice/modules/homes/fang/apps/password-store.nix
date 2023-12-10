{ config, pkgs, ... }:

{
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
}
