{ pkgs, ... }:

pkgs.mkShell {
  packages = with pkgs.pkgsUnrestricted; [
    hello
  ];
}
