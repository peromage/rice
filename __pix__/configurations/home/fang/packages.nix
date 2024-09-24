{ pix, pkgs, ... }:

{
  imports = [
    pix.nixosModules.homeManager
  ];

  /* Pre-configured packages */
  pix.homepkgs = {
    bash.enable = true;
    fcitx5.enable = true;
    fish.enable = true;
    git = {
      enable = true;
      extraIncludes = [
        { path = "${pix.path.dotfiles}/git/.config/git/user-fang"; }
      ];
    };
    gpg.enable = true;
    password-store.enable = true;
    powershell.enable = true;
    tmux.enable = true;
    zellij.enable = true;
    vim.enable = true;
    wezterm.enable = true;
    gnome = {
      enableKeyboardShortcuts = true;
      enableGnomeTerminalConfig = true;
    };
  };

  home.packages = with pkgs; [
    ## Daily
    pix.emacs
    pix.aspell
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
    nodejs_latest
    dotnet-sdk_8
    lua
    clang-tools
    shellcheck
    nixd
    pix.python3

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
    yt-dlp
    zbar

    ## GUI
    brave
    firefox
    remmina
    unrestricted.discord

    ## Wayland
    wl-clipboard
  ];
}
