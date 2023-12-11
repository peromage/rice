{ pkgs, rice, ... }:

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
  home.packages = with pkgs; [
    ## CLI
    fish
    ripgrep
    stow
    tmux
    git
    git-lfs

    ## Data transfer
    wget
    curl
    aria2
    rsync

    ## Coding
    emacs29
    vim
    graphviz

    ## Writing
    hunspell
    hunspellDicts.en-ca
    hunspellDicts.en-us
    ispell
    hugo

    ## Devices
    android-tools

    ## Security
    pinentry-gtk2
    gnupg

    ## Fancy stuff
    neofetch
    btop # Replace `htop'
    eza # Replace `ls'
    fzf
    jq # Json parser

    ## GUI
    brave
    firefox
    wezterm
    remmina

    ## Wayland
    wl-clipboard

    ## Development
    gnumake
    cmake
    python3
    libgcc
    nodejs_21
    dotnet-sdk_8
    lua
    libtool
  ];
}
