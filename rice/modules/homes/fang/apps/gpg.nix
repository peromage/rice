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

  ## Override with my own settings
  home.file = {
    ".gnupg" = {
      source = "${src}/.gnupg";
      recursive = true;
    };
  };
}
