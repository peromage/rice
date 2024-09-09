{ pix
, lib
, fetchFromGitHub
, system
, nixpkgs
, ... }:

let
  crane = builtins.getFlake "github:ipetkov/crane/7e4586bad4e3f8f97a9271def747cf58c4b68f3c"; # master September 3, 2024
  rust-overlay = builtins.getFlake "github:oxalica/rust-overlay/57a1564c924ee4acbffe0ad3d65c7e90d3e77cd8"; # master September 7, 2024

  pkgs = import nixpkgs {
    inherit system;
    overlays = [ (import rust-overlay) ];
  };

  src = fetchFromGitHub {
    owner = "alex35mil";
    repo = "dotfiles";
    rev = "67511131d40f6e0f03ed35f5ae3489cd1bfe2837"; # master on June 11, 2024
    hash = "sha256-p6yVwB/8peOHU5pX0oN0whsToHHmNQxI01ig5sKRx2I=";
  };

  rustWithWasiTarget = pkgs.rust-bin.stable.latest.default.override {
    extensions = [ "rust-src" "rust-std" "rust-analyzer" ];
    targets = [ "wasm32-wasi" ];
  };

  craneLib = (crane.mkLib pkgs).overrideToolchain rustWithWasiTarget;

in
craneLib.buildPackage {
  meta = {
    description = "Minimal statusbar plugin for Zellij.";
    homepage = "https://github.com/alex35mil/dotfiles/tree/master/bin/zellij/statusbar";
    license = lib.licenses.unlicense;
    maintainers = [ pix.maintainer ];
    platforms = lib.platforms.all;
  };

  src = craneLib.cleanCargoSource (craneLib.path "${src}/bin/zellij/statusbar");

  doCheck = false;
  doNotSign = true;
  cargoExtraArgs = "--target wasm32-wasi";
}
