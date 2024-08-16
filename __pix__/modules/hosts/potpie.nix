{ config, lib, pkgs, ... }:

let
  cfg = config.pix.hosts.profiles.potpie;

in {
  options.pix.hosts.profiles.potpie = with lib; {
    enable = mkEnableOption "host potpie";
  };

  config = lib.mkIf cfg.enable {
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
        dejavu_fonts
        wqy_zenhei
        wqy_microhei
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
            "WenQuanYi Zen Hei Mono"
            "DejaVu Sans Mono"
          ];

          sansSerif = [
            "Noto Sans CJK SC"
            "Noto Sans CJK TC"
            "WenQuanYi Zen Hei"
            "Dejavu Sans"
          ];

          serif = [
            "Noto Serif CJK SC"
            "Noto Serif CJK TC"
            "WenQuanYi Zen Hei"
            "Dejavu Serif"
          ];
        };
      };
    };

    programs.nix-ld = {
      enable = true;
      libraries = with pkgs; [];
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
  };
}
