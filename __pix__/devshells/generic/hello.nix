{ pkgs, ... }:

pkgs.mkShell {
  packages = with pkgs; [ hello ];
}
