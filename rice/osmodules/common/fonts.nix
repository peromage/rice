{ pkgs, ... }:

{
  fonts.fonts = with pkgs; [
    iosevka
    cascadia-code
    emacs-all-the-icons-fonts
    nerdfonts
    liberation_ttf
    noto-fonts
    noto-fonts-emoji
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-lgc-plus
  ];
}
