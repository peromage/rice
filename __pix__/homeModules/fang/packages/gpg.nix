{ config, lib, pkgs, pix, ... }:

let
  cfg = config.pix.homepkgs.gpg;
  src = "${pix.path.dotfiles}/gnupg/.gnupg";

in with lib; {
  options.pix.homepkgs.gpg = {
    enable = mkEnableOption "GPG";
  };

  config = mkIf cfg.enable {
    programs.gpg = {
      enable = true;
      scdaemonSettings = {};
    };

    services.gpg-agent = {
      enable = true;
      enableScDaemon = true;
      enableSshSupport = true;
      enableBashIntegration = true;
      enableFishIntegration = true;
      # pinentryPackage = pkgs.pinentry-gnome3;
    };

    home.packages = with pkgs; [
      pinentry-gnome3 # Only one pinentry package at a time, conflicts otherwise
    ];

    ## Workaround to prevent SSH_AUTH_SOCK being set with wrong value
    ## Ref: https://wiki.archlinux.org/title/GNOME/Keyring#Disabling
    xdg.configFile."autostart/gnome-keyring-ssh.desktop".text = ''
      [Desktop Entry]
      Name=SSH Key Agent
      Type=Application
      Hidden=true
    '';

    ## Override with my own settings
    home.file.".gnupg" = {
      source = src;
      recursive = true;
    };
  };
}
