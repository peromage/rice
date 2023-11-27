{ pkgs, ... }:

{
  i18n = {
    defaultLocale = "en_US.UTF-8";
    inputMethod.enabled = "fcitx5";
    inputMethod.fcitx5.addons = with pkgs; [
      fcitx5-rime
      fcitx5-configtool
      fcitx5-chinese-addons
      fcitx5-gtk
    ];
  };

  console = {
    font = "Lat2-Terminus16";
    useXkbConfig = true; # use xkbOptions in tty.
  };

  time.timeZone = "America/Detroit";
}
