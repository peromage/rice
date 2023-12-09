{ rice, ... }:

let
  src = "${rice.dirs.dotfiles}/gnupg";

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
