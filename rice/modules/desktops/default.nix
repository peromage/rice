{ config, lib, rice, ... }:

let
  librice = rice.lib;
  cfg = config.rice.desktops;

  /* Additional arguments to import submodules.

     CONTRACT: Each profile declared in this set must have options:

       - enable
  */
  args = {
    mkDesktopOptions = { name }: with lib; {
      enable = mkEnableOption "desktop environment";
    };
  };

  ## Don't enable display server if no desktop environment is enabled
  enableDisplayServer = lib.foldlAttrs (a: _: v: v.enable || a) false cfg.env;

in with lib; {
  imports = with librice; callListWithArgs args (allButDefault ./.);

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

    env = {};
  };

  config = mkIf enableDisplayServer {
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
