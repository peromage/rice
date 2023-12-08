{ pkgs, ... }:

{
  home.packages = with pkgs; [
    ## Coding
    emacs
    vim
    graphviz

    ## Devices
    android-tools

    ## Security
    pinentry
    gnupg
    pass
    passExtensions.pass-otp
    passExtensions.pass-genphrase

    ## CLI
    fish
    ispell
    ripgrep
    stow
    tmux
    wget
    curl
    rsync
    git
    git-lfs
    unzip

    ## GUI
    brave
    firefox
    wezterm
    remmina

    ## Wayland
    wl-clipboard
  ];
}
