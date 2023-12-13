{ pkgs, ... }:

let
  useEmacsWith = with pkgs; (emacsPackagesFor emacs29).emacsWithPackages;

  /* Refs:
     https://github.com/martinbaillie/dotfiles/blob/c31b8aa815a940c45210c7ab9029141a8e6c6e93/modules/editors/emacs.nix#L30
     https://nixos.wiki/wiki/Emacs
  */
  emacsPackages = epkgs: with epkgs; [
    vterm # Since vterm cannot be compiled in user environment, use this instead
  ];

in {
  home.packages = with pkgs; [
    (useEmacsWith emacsPackages)
    libvterm-neovim # libvterm is not maintained, use this instead
  ];
}
