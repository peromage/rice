/* C/C++ build tools/dependencies.

   The name is brought from the similar package in Debian.
*/

{ pkgs, ... }:

pkgs.buildEnv {
  name = "build-essential";
  paths = with pkgs; [
    gcc
    libgcc
    gnumake
    cmake
    autoconf
    libtool
    linuxHeaders
  ];
}
