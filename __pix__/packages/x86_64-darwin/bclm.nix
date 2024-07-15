{ stdenv
, lib
, fetchFromGitHub
, swift
, swiftPackages
, ... }:

swiftPackages.stdenv.mkDerivation rec {
  pname = "bclm";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "zackelia";
    repo = "bclm";
    rev = "v0.1.0";
    sha256 = "sha256-PLdbQZr4u5RCriyO9F7HaK95TupOMy9DLIjiqakRQEs=";
  };

  buildInputs = with swiftPackages; [
    swift
    swiftpm
    swift-driver
    swift-docc
    Foundation
    xcbuild
    XCTest ];
}
