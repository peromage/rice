{ pix, pkgs, ... }:

{
  imports = [
    pix.nixosModules.homeManager
  ];

  /* Pre-configured packages */
  pix.homeprogs = {
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
    python.enable = true;
    nodejs.enable = true;
  };

  home.packages = with pkgs; [
    ## Daily
    pixPkgs.emacs
    pixPkgs.aspell
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
    dotnet-sdk_8
    lua
    clang-tools
    shellcheck
    nixd
    pixPkgs.build-essential

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
    zeal

    ## Wayland
    wl-clipboard
  ];
}
