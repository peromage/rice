{ pix, pkgs, ... }:

let
  src = "${pix.paths.dotfiles}/pwsh/.config/powershell";

in {
  home.packages = [ pkgs.powershell ];

  xdg.configFile."powershell" = {
    source = src;
    recursive = true;
  };
}
