{ pkgs, ... }:

pkgs.mkShell {
  packages = with pkgs.unrestrictedPkgs; [
    hello
  ];
}
