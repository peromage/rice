/* Nixified Gnome dconf configurations.

   To reliably convert dumped dconf database to home-manager config, use `dconf2nix'.

   1. Make modifications through Gnome menus.
   2. Dump and nixify dconf: dconf dump / | dconf2nix > dconf.nix
   3. Cherry-pick config.
*/

{ config, lib, pkgs, ...}:

let
  cfg = config.pix.homepkgs.gnome;
  dconfDump = import ./dconf.nix { inherit lib; };

in with lib; {
  options.pix.homepkgs.gnome = {
    enableKeyboardShortcuts = mkEnableOption "Customized keyboard shortcut";
    enableGnomeTerminalConfig = mkEnableOption "Customized Gnome Terminal config";
  };

  config = mkMerge [
    {
      home.packages = (with pkgs; [
        dconf2nix
      ]);
    }

    (mkIf cfg.enableKeyboardShortcuts {
      dconf.settings = {
        inherit (dconfDump.dconf.settings)
          "org/gnome/desktop/wm/keybindings"
          "org/gnome/mutter/wayland/keybindings"
          "org/gnome/settings-daemon/plugins/media-keys"
          "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0"
          "org/gnome/shell/keybindings";
      };
    })

    (mkIf cfg.enableGnomeTerminalConfig {
      dconf.settings = {
        inherit (dconfDump.dconf.settings)
          "org/gnome/terminal/legacy"
          "org/gnome/terminal/legacy/keybindings"
          "org/gnome/terminal/legacy/profiles:"
          "org/gnome/terminal/legacy/profiles:/:233c6191-db1e-403e-9b76-0f006019cf4c" ;
      };
    })
  ];
}
