{ pkgs, ... }:

{
  programs.bash = {
    enable = true;
    enableCompletion = true;
  };

  programs.zsh = {
    enable = true;
    enableBashCompletion = true;
    enableCompletion = true;
    enableFzfCompletion = true;
  };

  programs.fish = {
    enable = true;
    vendor.completions.enable = true;
  };

  environment.systemPackages = with pkgs; [
    vim
    tmux
    git
    git-lfs
    rsync
    curl
    wget
    tree
    emacs29
    fzf
  ];
}
