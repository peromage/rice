{ pix, pkgs, ... }:

let
  src = "${pix.path.dotfiles}/pwsh/.config/powershell";

in {
  home.packages = [ pkgs.powershell ];

  xdg.configFile."powershell" = {
    source = src;
    recursive = true;
  };
}
