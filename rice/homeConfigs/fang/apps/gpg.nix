{ pkgs, rice, ... }:

let
  src = "${rice.paths.dotfiles}/gnupg";

in {
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
  };

  home.packages = with pkgs; [
    pinentry-gtk2 # Only one pinentry package at a time, conflicts otherwise
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
    source = "${src}/.gnupg";
    recursive = true;
  };
}
