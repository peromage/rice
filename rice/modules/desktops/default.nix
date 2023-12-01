{ lib, rice, ... }:

let
  librice = rice.lib;

in with lib; {
  imports = librice.allButDefault ./.;

  options.rice.desktops = {
    /* The display server is actually selected by the display manager.
       See: https://discourse.nixos.org/t/enabling-x11-still-results-in-wayland/25362/2
    */
    enableWayland = mkOption {
      type = types.bool;
      default = true;
      description = "Enable Wayland as default display server.";
    };

    enableOpenGL = mkOption {
      type = types.bool;
      default = true;
      description = "Enable OpenGL support.";
    };

    env = {
      gnome = {
        enable = mkEnableOption "Enable Gnome desktop environment.";
        disableGDM = mkEnableOption "Disable GDM.";
      };

      kde = {
        enable = mkEnableOption "Enable KDE Plasma desktop environment.";
        disableSDDM = mkEnableOption "Disable SDDM.";
      };
    };
  };
}
