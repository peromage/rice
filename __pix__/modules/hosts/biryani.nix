{ pkgs, ... }:

{
  config.pix.hosts.profiles.biryani = {
    config = {
      environment.systemPackages = with pkgs; [
        firefox
        vim
        tmux
        git
        curl
        wget
        rsync
        tree

        ## Filesystems
        ntfs3g
        exfat
        exfatprogs
        e2fsprogs
        fuse
        fuse3
      ];
    };
  };
}
