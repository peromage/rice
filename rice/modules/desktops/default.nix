{ config, lib, rice, ... }:

let
  cfg = config.rice.desktops;

  options = with lib; {
    /* The display server is actually selected by the display manager.
       See: https://discourse.nixos.org/t/enabling-x11-still-results-in-wayland/25362/2
    */
    enableWayland = mkEnableOption "Wayland display server" // { default = true; };
    enableOpenGL = mkEnableOption "OpenGL support" // { default = true; };

    env = {};
  };

  /* Additional arguments to import submodules.

     CONTRACT: Each profile declared in this set must have options:

       - enable
  */
  args = with lib; {
    mkDesktopOptions = { name }: {
      enable = mkEnableOption "desktop environment";
    };
  };

  librice = rice.lib;

  ## Do not enable desktop settings if no desktop environment is enabled
  enableDesktopConfig = librice.anyEnable cfg.env;

in {
  imports = with librice; callListWithArgs args (listDirNoDefault ./.);
  options.rice.desktops = options;

  config = with lib; mkIf enableDesktopConfig {
    services.xserver = {
      enable = true;
      libinput.enable = true;
    };
    programs.xwayland.enable = cfg.enableWayland;
    hardware.opengl = {
      enable = cfg.enableOpenGL;
      driSupport = true;
      driSupport32Bit = true;
    };
  };
}
