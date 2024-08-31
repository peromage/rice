/* Nixified Gnome dconf configurations.

   To reliably convert dumped dconf database to home-manager config, use `dconf2nix'.

   1. Make modifications through Gnome menus.
   2. Dump and nixify dconf: dconf dump / | dconf2nix > dconf.nix
   3. Cherry-pick config.
*/

{ config, lib, pix, ...}:

let
  cfg = config.pix.homepkgs.gnome;
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
      "org/gnome/desktop/wm/keybindings" = {
        begin-resize = [];
        cycle-group = [];
        cycle-group-backward = [];
        cycle-panels = [];
        cycle-panels-backward = [];
        cycle-windows = [];
        cycle-windows-backward = [];
        maximize-horizontally = [ "<Shift><Super>Left" ];
        maximize-vertically = [ "<Shift><Super>Up" ];
        minimize = [ "<Super>d" ];
        move-to-monitor-down = [];
        move-to-monitor-left = [];
        move-to-monitor-right = [];
        move-to-monitor-up = [];
        move-to-workspace-1 = [];
        move-to-workspace-last = [];
        move-to-workspace-left = [ "<Shift><Super>m" ];
        move-to-workspace-right = [ "<Super>m" ];
        panel-run-dialog = [ "<Super>r" ];
        switch-applications = [];
        switch-applications-backward = [];
        switch-panels = [];
        switch-panels-backward = [];
        switch-to-workspace-1 = [];
        switch-to-workspace-last = [];
        switch-to-workspace-left = [ "<Super>b" ];
        switch-to-workspace-right = [ "<Super>f" ];
        switch-windows = [ "<Alt>Tab" ];
        switch-windows-backward = [ "<Shift><Alt>Tab" ];
        toggle-maximized = [];
      };

      "org/gnome/mutter/wayland/keybindings" = {
        restore-shortcuts = [];
      };

      "org/gnome/settings-daemon/plugins/media-keys" = {
        custom-keybindings = [ "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/" ];
        magnifier = [];
        magnifier-zoom-in = [];
        magnifier-zoom-out = [];
        screenreader = [];
        screensaver = [ "<Super>l" ];
      };

      "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
        binding = "<Super>t";
        command = "gnome-terminal";
        name = "Gnome Terminal";
      };

      "org/gnome/shell/keybindings" = {
        focus-active-notification = [];
        toggle-message-tray = [];
        toggle-overview = [];
        toggle-quick-settings = [];
      };
    })

    (mkIf cfg.enableGnomeTerminalConfig {
      "org/gnome/terminal/legacy/keybindings" = {
        close-tab = "disabled";
        close-window = "disabled";
        find = "<Primary><Shift>f";
        find-clear = "disabled";
        find-next = "disabled";
        find-previous = "disabled";
        full-screen = "disabled";
        move-tab-left = "disabled";
        move-tab-right = "disabled";
        new-tab = "<Primary><Shift>t";
        new-window = "<Primary><Shift>n";
        next-tab = "<Primary>grave";
        prev-tab = "<Primary>asciitilde";
        switch-to-tab-1 = "disabled";
        switch-to-tab-10 = "disabled";
        switch-to-tab-2 = "disabled";
        switch-to-tab-3 = "disabled";
        switch-to-tab-4 = "disabled";
        switch-to-tab-5 = "disabled";
        switch-to-tab-6 = "disabled";
        switch-to-tab-7 = "disabled";
        switch-to-tab-8 = "disabled";
        switch-to-tab-9 = "disabled";
        zoom-in = "disabled";
        zoom-normal = "disabled";
        zoom-out = "disabled";
      };

      "org/gnome/terminal/legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9" = {
        cursor-blink-mode = "off";
        custom-command = "fish -i";
        font = "Iosevka 12";
        text-blink-mode = "always";
        use-custom-command = true;
        use-system-font = false;
        use-theme-colors = true;
        visible-name = "my_fish";
      };
    })
  ];
}
