{ config, lib, pkgs, rice, ... }:

let
  librice = rice.lib;
  cfg = config.rice.hosts.profiles.potpie;

in lib.mkIf cfg.enable {
  /* Locale */
  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/Detroit";

  console = {
    enable = true;
    earlySetup = false;
    /* There is a bug where font is set other than default the null, for
       example, "Lat2-Terminus16", the `systemd-vconsole-setup.service' will
       fail to start in stage 1.
       Ref: https://github.com/NixOS/nixpkgs/issues/257904
    */
    font = null;
    useXkbConfig = true; # use xkbOptions in tty.
  };

  /* Sound */
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  /* Network */
  networking = {
    networkmanager.enable = true;
    useDHCP = lib.mkDefault true;
  };

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
