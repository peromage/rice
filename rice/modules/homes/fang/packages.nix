{ pkgs, ... }:

{
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
    emacs
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
    pinentry
    gnupg
    (pass.withExtensions (exts: with exts; [
      pass-otp
      pass-genphrase
    ]))

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
  ];
}
