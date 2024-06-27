{ pkgs, ... }:

{
  home.packages = (with pkgs; [
    ## CLI
    ripgrep
    stow
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
