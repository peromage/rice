{ config, lib, ... }:

let
  cfg = config.pix.hardware.audio;

  options = {
    enable = lib.mkEnableOption "audio services";
  };

in {
  options.pix.hardware.audio = options;

  config = lib.mkIf cfg.enable {
    sound = {
      enable = false; # No ALSA sound
      mediaKeys.enable = false; # Gnome and KDE has their own handling
    };

    hardware.pulseaudio.enable = false; # Use pipewire
    security.rtkit.enable = true;

    services.pipewire = {
      enable = true;
      systemWide = false; # Not recommended by official news
      audio.enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      # If you want to use JACK applications, uncomment this
      jack.enable = true;

      # use the example session manager (no others are packaged yet so this is enabled by default,
      # no need to redefine it in your config for now)
      #media-session.enable = true;
    };
  };
}
