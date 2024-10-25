## My dotfiles

{ pkgs, pix, ... }:

pkgs.stdenvNoCC.mkDerivation {
  pname = "ricepot";
  version = "0.0.1";
  src = pix.path.dotfiles;

  nativeBuildInputs = with pkgs; [ rsync stow ];

  installPhase = ''
    mkdir -p $out/dotfiles
    rsync -av $src/* $out/dotfiles/
    stow --no-folding --target=$out --dir=$out/dotfiles bin emacs
  '';
}
