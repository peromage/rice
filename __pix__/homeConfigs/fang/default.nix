{ pix, pkgs, ... }:

let
  src = pix.paths.dotfiles;

in {
  imports = [ ./packages.nix ];

  /* Managed by Home Manager */
  programs.home-manager.enable = true;


  /* Mapped paths from Home Manager's variables:

     `~/.config': config.xdg.configHome
     `~/.local/share': config.xdg.dataHome

     Shorthands for creating files under directories:

     `~': home.file.<name>
     Ref: https://nix-community.github.io/home-manager/options.html#opt-home.file

     `~/.config': xdg.configFile.<name>
     Ref: https://nix-community.github.io/home-manager/options.html#opt-xdg.configFile

     `~/.local/share': xdg.dataFile.<name>
     Ref: https://nix-community.github.io/home-manager/options.html#opt-xdg.dataFile
  */
  xdg.enable = true;

  home = {
    username = "fang";
    homeDirectory = "/home/fang";
    stateVersion = "24.05";
  };

  /* Alternatively source in a manual way:
     ~/.nix-profile/etc/profile.d/hm-session-vars.sh
     or
     /etc/profiles/per-user/fang/etc/profile.d/hm-session-vars.sh
  */
  home.sessionVariables = {
    EDITOR = "vim";
  };

  home.sessionPath = [
    "\${HOME}/bin"
    "\${HOME}/.local/bin"
  ];

  home.file = {
    "bin" = {
      source = "${src}/bin/bin";
      recursive = true;
    };
  };
}
