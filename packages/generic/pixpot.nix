## My dotfiles

{ pkgs, pix, ... }:

pkgs.stdenvNoCC.mkDerivation {
  pname = "pixpot";
  version = "0.0.1";
  src = pix.path.dotfiles;

  nativeBuildInputs = with pkgs; [ rsync stow ];

  installPhase = ''
    mkdir -p $out/dotfiles
    rsync -a $src/* $out/dotfiles/
    stow --no-folding --target=$out --dir=$out/dotfiles bin
  '';
}
