{ config, lib, ... }:

let
  cfg = config.rice.desktops;

  ## Don't enable display server if no desktop environment is enabled
  enableDisplayServer = lib.foldlAttrs (a: _: v: v.enable || a) false cfg.env;

in lib.mkIf enableDisplayServer {
  services.xserver = {
    enable = true;
    libinput.enable = true;
  };
  programs.xwayland.enable = cfg.enableWayland;
  hardware.opengl.enable = cfg.enableOpenGL;
}
