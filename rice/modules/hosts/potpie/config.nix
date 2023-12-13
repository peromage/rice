{ config, lib, pkgs, rice, ... }:

let
  nixpkgs = rice.nixpkgs;
  librice = rice.lib;
  cfg = config.rice.hosts.hosts.potpie;

in lib.mkIf cfg.enable {
  /* Locale */
  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  console = {
    font = "Lat2-Terminus16";
    useXkbConfig = true; # use xkbOptions in tty.
  };

  time.timeZone = "America/Detroit";

  /* Sound */
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  /* Network */
  networking = {
    networkmanager.enable = true;
    useDHCP = lib.mkDefault true;
    firewall.enable = true;
  };

  /* Nix settings */
  nixpkgs = {
    hostPlatform = lib.mkDefault "x86_64-linux";
    config = {
      allowUnfree = true;
      allowBroken = true;
    };
  };

  nix = {
    ## Enable experimental features
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      auto-optimise-store = true;
    };

    optimise = {
      automatic = true;
      dates = [ "weekly" ];
    };

    gc = {
      automatic = false; # Manual GC
      dates = "weekly";
      options = "--delete-older-than 30d";
    };

    ## Synonyms
    ## pkgs.nixVersions.stable -> pkgs.nix, pkgs.nixFlakes, pkgs.nixStable
    ## pkgs.nixVersions.unstable -> pkgs.nixUnstable
    ## See: https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/aliases.nix
    package = pkgs.nixFlakes;

    ## Use the nixpkgs from the toplevel flake
    registry.nixpkgs.flake = nixpkgs;
    nixPath = [ "/etc/nix/path" ];
  };

  environment.etc."nix/path/nixpkgs".source = "${nixpkgs}";

  /* Fonts */
  fonts = {
    fontDir.enable = true;

    packages = with pkgs; [
      iosevka
      cascadia-code
      emacs-all-the-icons-fonts
      nerdfonts
      liberation_ttf
      noto-fonts
      noto-fonts-emoji
      noto-fonts-cjk-sans
      noto-fonts-cjk-serif
      noto-fonts-lgc-plus
    ];

    fontconfig = {
      enable = true;
      includeUserConf = true;
      antialias = true;

      hinting = {
        enable = true;
        style = "slight";
      };

      defaultFonts= {
        emoji = [
          "Noto Color Emoji"
        ];

        monospace = [
          "Cascadia Code"
          "Iosevka"
          "DejaVu Sans Mono"
        ];

        sansSerif = [
          "Noto Sans CJK SC"
          "Noto Sans CJK TC"
          "Dejavu Sans"
        ];

        serif = [
          "Noto Serif CJK SC"
          "Noto Serif CJK TC"
          "Dejavu Serif"
        ];
      };
    };
  };

  /* Packages */
  environment.systemPackages = with pkgs; [
    ## Most used CLI
    vim
    wget
    curl
    rsync
    git
    git-lfs
    coreutils
    pinentry
    gnupg
    tmux
    zstd
    tree
    pinentry
    gnupg

    ## Archive
    zip
    unzip
    xz
    p7zip

    ## Filesystem
    ntfs3g
    exfat
    exfatprogs
    e2fsprogs
    fuse
    fuse3

    ## Monitors
    htop # CPU and memory
    iotop # IO
    iftop # Network

    ## Devices
    parted
    pciutils # lspci
    usbutils # lsusb
    lsof # List open files
    dnsutils  # dig, nslookup
    iperf3
    nmap

    ## Apps
    appimage-run

    ## Development
    gcc
    libgcc
    gnumake
    cmake
    autoconf
    libtool
  ];
}
