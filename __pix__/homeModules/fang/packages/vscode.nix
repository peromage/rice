{ pix, ... }:

let
  src = "${pix.path.dotfiles}/vscode/.config/Code";

in {
  programs.vscode.enable = true;

  xdg.configFile."Code" = {
    source = src;
    recursive = true;
  };
}
