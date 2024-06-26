{ pkgs, ... }:

{
  imports = [ ./apps ];

  /* Home settings */
  programs.home-manager.enable = true;

  home = {
    username = "fang";
    homeDirectory = "/home/fang";
    stateVersion = "23.11";
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

  /* Packages */
  home.packages = (with pkgs; [
    ## CLI
    vim
    fish
    ripgrep
    stow
    tmux
    git
    git-lfs
    powershell
    ## Data transfer
    wget
    curl
    aria2
    rsync
    ## Fancy stuff
    neofetch
    btop # Replace `htop'
    eza # Replace `ls'
    fzf
    jq # Json parser

    ## Development
    python3
    nodejs_latest
    dotnet-sdk_8
    lua

    ## Devices
    android-tools

    ## Productivity
    graphviz
    hugo
    libreoffice-fresh
    gimp
    kdenlive
    flameshot
    diceware

    ## GUI
    brave
    firefox
    wezterm
    remmina

    ## Wayland
    wl-clipboard

  ]) ++ (with pkgs.ricePkgs; [
    ## Editors
    emacs
    aspell

  ]) ++ (with pkgs.unrestrictedPkgs; [
    ## Gaming
    discord

  ]);
}
