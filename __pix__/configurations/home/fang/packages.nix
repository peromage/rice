{ pix, pkgs, ... }:

{
  imports = [
    pix.nixosModules.home-manager
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
    vim.enable = true;
    # wezterm.enable = true;
  };

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
    nodejs_latest
    dotnet-sdk_8
    lua
    clang-tools
    shellcheck
    nixd

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
  ])

  ++ (with pkgs.pixPkgs; [
    ## Editors
    emacs29
    aspell

    ## Dev
    python3
  ])

  ++ (with pkgs.unrestrictedPkgs; [
    ## Gaming
    discord
  ]);
}
