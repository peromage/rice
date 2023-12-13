{ pkgs, ... }:

pkgs.mkShell {
  packages = with pkgs.pkgsCustom; [
    hello
  ];
}
