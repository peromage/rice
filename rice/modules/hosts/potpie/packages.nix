{ pkgs, ... }:

{
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
  ];
}
