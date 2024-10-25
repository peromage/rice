{ pkgs, ... }:

let
  emacs = pkgs.emacs29;

  /* Emacs dependencies.  Some packages like vterm may have issues to build in
     Nix environment due to encapsulated environment.  Include them together
     with this declaration so that Emacs can find them.

     Refs:
     https://github.com/martinbaillie/dotfiles/blob/c31b8aa815a940c45210c7ab9029141a8e6c6e93/modules/editors/emacs.nix#L30
     https://nixos.wiki/wiki/Emacs
  */
  myEmacs = (pkgs.emacsPackagesFor emacs).emacsWithPackages (epkgs: with epkgs; [
    vterm # Since vterm cannot be compiled in user environment, use this instead
  ]);

  ## May omit buildEnv?
in pkgs.buildEnv {
  name = "my-emacs";
  paths = with pkgs; [
    myEmacs
    ripgrep
    libvterm-neovim # libvterm is not maintained, use this instead
  ];
}
