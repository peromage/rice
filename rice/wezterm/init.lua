-- -*- lua-indent-level: 4; tab-width: 4; -*-
local wezterm = require "wezterm"
local act = wezterm.action

local rice_keys = {
    -- Active all the time
    { mods = "CTRL",        key = "Tab",    action = act.ActivateTabRelative(1) },
    { mods = "CTRL|SHIFT",  key = "Tab",    action = act.ActivateTabRelative(-1) },
    { mods = "CTRL|SHIFT",  key = "C",      action = act.CopyTo "Clipboard" },
    { mods = "CTRL|SHIFT",  key = "V",      action = act.PasteFrom "Clipboard" },
    { mods = "CTRL|SHIFT",  key = "S",      action = act.QuickSelect },
    { mods = "CTRL|SHIFT",  key = "U",      action = act.CharSelect{ copy_on_select = true, copy_to = "ClipboardAndPrimarySelection" }},
    -- Most of the less used commands can be accessed from the palette or the launcher
    -- { mods = "CTRL|SHIFT",  key = "P",      action = act.ActivateCommandPalette },
    { mods = "CTRL|SHIFT",  key = "L",      action = act.ShowLauncher },
    { mods = "ALT",         key = "Enter",  action = act.ToggleFullScreen },
    -- Mode shift
    { mods = "CTRL|SHIFT",  key = "F",      action = act.ActivateCopyMode },
    { mods = "CTRL|SHIFT",  key = "Space",  action = act.ActivateKeyTable{ name = "rice_transient_mode_table", one_shot = false, timeout_milliseconds = 1000 }},
}
local rice_transient_mode_table = {
    -- Exit keys
    { mods = "NONE",  key = "Escape",  action = act.PopKeyTable },
    { mods = "CTRL",  key = "g",       action = act.PopKeyTable },
    -- Tabs
    { mods = "NONE",  key = "t",       action = act.SpawnTab "CurrentPaneDomain" },
    { mods = "NONE",  key = "Q",       action = act.CloseCurrentTab{ confirm = true }},
    { mods = "NONE",  key = "f",       action = act.ActivateTabRelative(1) },
    { mods = "NONE",  key = "b",       action = act.ActivateTabRelative(-1) },
    -- Panes
    { mods = "NONE",  key = "2",       action = act.SplitVertical{ domain = "CurrentPaneDomain" }},
    { mods = "NONE",  key = "3",       action = act.SplitHorizontal{ domain = "CurrentPaneDomain" }},
    { mods = "NONE",  key = "z",       action = act.TogglePaneZoomState },
    { mods = "NONE",  key = "o",       action = act.PaneSelect{ mode =  "Activate" }},
    { mods = "NONE",  key = "q",       action = act.CloseCurrentPane{ confirm = true }},
    -- Windows
    { mods = "NONE",  key = "N",       action = act.SpawnWindow },
    -- Workspace
    { mods = "NONE",  key = "n",       action = act.SwitchWorkspaceRelative(1) },
    { mods = "NONE",  key = "p",       action = act.SwitchWorkspaceRelative(-1) },
    -- Keys that are used repeatedly most of the time
    { mods = "CTRL",  key = "0",       action = act.ResetFontSize },
    { mods = "CTRL",  key = "=",       action = act.IncreaseFontSize },
    { mods = "CTRL",  key = "-",       action = act.DecreaseFontSize },
    { mods = "CTRL",  key = "h",       action = act.AdjustPaneSize{ "Left",   1}},
    { mods = "CTRL",  key = "j",       action = act.AdjustPaneSize{ "Down",   1}},
    { mods = "CTRL",  key = "k",       action = act.AdjustPaneSize{ "Up",     1}},
    { mods = "CTRL",  key = "l",       action = act.AdjustPaneSize{ "Right",  1}},
    { mods = "CTRL",  key = "o",       action = act.RotatePanes "Clockwise" },
}

local rice_copy_mode_table = {
    -- Exit
    { mods = "NONE",   key = "q",       action = act.CopyMode "Close" },
    { mods = "CTRL" ,  key = "g",       action = act.CopyMode "Close" },
    -- Cursor movement
    { mods = "NONE",   key = "h",       action = act.CopyMode "MoveLeft" },
    { mods = "NONE",   key = "j",       action = act.CopyMode "MoveDown" },
    { mods = "NONE",   key = "k",       action = act.CopyMode "MoveUp" },
    { mods = "NONE",   key = "l",       action = act.CopyMode "MoveRight" },
    { mods = "NONE",   key = "w",       action = act.CopyMode "MoveForwardWord" },
    -- { mods = "NONE",   key = "e",       action = act.CopyMode "MoveForwardWordEnd" },
    { mods = "NONE",   key = "b",       action = act.CopyMode "MoveBackwardWord" },
    { mods = "NONE",   key = "0",       action = act.CopyMode "MoveToStartOfLine" },
    { mods = "SHIFT",  key = "^",       action = act.CopyMode "MoveToStartOfLine" },
    { mods = "SHIFT",  key = "_",       action = act.CopyMode "MoveToStartOfLineContent" },
    { mods = "SHIFT",  key = "$",       action = act.CopyMode "MoveToEndOfLineContent" },
    { mods = "NONE",   key = "g",       action = act.CopyMode "MoveToScrollbackTop" },
    { mods = "SHIFT",  key = "G",       action = act.CopyMode "MoveToScrollbackBottom" },
    -- Scroll
    { mods = "CTRL",   key = "b",       action = act.CopyMode "PageUp" },
    { mods = "CTRL",   key = "f",       action = act.CopyMode "PageDown" },
    { mods = "CTRL",   key = "e",       action = act.CopyMode "MoveToViewportTop" },
    { mods = "CTRL",   key = "y",       action = act.CopyMode "MoveToViewportBottom" },
    { mods = "CTRL",   key = "l",       action = act.CopyMode "MoveToViewportMiddle" },

    -- Selection
    { mods = "NONE" ,  key = "v",       action = act.CopyMode{ SetSelectionMode = "Cell" }},
    { mods = "CTRL" ,  key = "v",       action = act.CopyMode{ SetSelectionMode = "Block" }},
    { mods = "SHIFT",  key = "V",       action = act.CopyMode{ SetSelectionMode = "Line" }},
    { mods = "NONE" ,  key = "y",       action = act.CopyTo "ClipboardAndPrimarySelection" },
    { mods = "NONE" ,  key = "u",       action = act.CopyMode "ClearSelectionMode" },
    { mods = "NONE" ,  key = "Escape",  action = act.CopyMode "ClearSelectionMode" },

    -- Search
    { mods = "NONE",   key = "f",       action = act.CopyMode{ JumpForward = { prev_char = false }}},
    { mods = "SHIFT",  key = "F",       action = act.CopyMode{ JumpBackward = { prev_char = false }}},
    { mods = "NONE",   key = "t",       action = act.CopyMode{ JumpForward = { prev_char = true }}},
    { mods = "SHIFT",  key = "T",       action = act.CopyMode{ JumpBackward = { prev_char = true }}},
    -- Switch to the search mode directly since the "EditPattern" has conflicts
    -- with the CopyMode at this moment (Hope it will be fixed in the future)
    { mods = "NONE",   key = "/",       action = act.Search "CurrentSelectionOrEmptyString" },
    { mods = "SHIFT",  key = "#",       action = act.CopyMode "ClearPattern" },
    { mods = "NONE",   key = "n",       action = act.CopyMode "NextMatch" },
    { mods = "SHIFT",  key = "N",       action = act.CopyMode "PriorMatch" },
    { mods = "NONE",   key = ",",       action = act.CopyMode "JumpReverse" },
    { mods = "NONE",   key = ";",       action = act.CopyMode "JumpAgain" },
}

local rice_search_mode_table = {
    -- Switch back to the CopyMode when the search pattern is accepted or canceled
    -- Avoid accidentally exiting the CopyMode
    { mods = "NONE",  key = "Enter",   action = act.ActivateCopyMode },
    { mods = "NONE",  key = "Escape",  action = act.ActivateCopyMode },
    { mods = "CTRL",  key = "g",       action = act.ActivateCopyMode },
    -- Emacs style
    { mods = "CTRL",  key = "i",       action = act.CopyMode "CycleMatchType" },
    { mods = "CTRL",  key = "s",       action = act.CopyMode "NextMatch" },
    { mods = "CTRL",  key = "r",       action = act.CopyMode "PriorMatch" },
    { mods = "CTRL",  key = "k",       action = act.CopyMode "ClearPattern" },
}

local rice_launcher_menu = {
    {
        label = "Pwsh",
        args = { "pwsh", "-NoLogo" },
    },
    {
        label = "Bash",
        args = { "bash", "-i" },
    }
}

-- Since the config for Wezterm cannot have any function property, use meta table
-- to provide some additional functionalities.
local riceconf_meta = {
    -- Bind the meta table
    rice_bind = function(self, conf)
        setmetatable(conf, self)
        self.__index = self
        return conf
    end,

    -- A utility function to merge a list of tables to the current one in place.
    -- Configs in the later tables will overwrite the former ones if they share
    -- the same keys.
    -- Return self after merging.
    rice_merge = function(self, ...)
        for _,tbl in ipairs({...}) do
            for k,v in pairs(tbl) do
                self[k] = v
            end
        end
        return self
    end,

    -- Make some modifications on a builtin color scheme and return a new color
    -- scheme object.
    rice_color_scheme = function(scheme_name)
        local scheme = wezterm.get_builtin_color_schemes()[scheme_name]
        -- Make the scrollbar more visible (lightness less than 0.6 considered as
        -- a dark theme)
        local h, s, l, a = wezterm.color.parse(scheme.background):hsla()
        scheme.scrollbar_thumb = wezterm.color.from_hsla(h, s, l < 0.6 and 1 or 0, a)
        return scheme
    end,

    -- Some meta data
    rice_platform = wezterm.target_triple == "x86_64-pc-windows-msvc" and "win" or "*nix",
}

local riceconf = {
    -- System
    check_for_updates = false,
    automatically_reload_config = false,
    adjust_window_size_when_changing_font_size = false,
    audible_bell = "Disabled",
    -- Sometimes exiting SSH doesn't return 0 and it's annoying to manually
    -- close the window
    exit_behavior = "Close",
    window_close_confirmation = 'AlwaysPrompt',
    hide_mouse_cursor_when_typing = true,
    enable_wayland = true,
    front_end = "OpenGL",
    webgpu_power_preference = "LowPower",
    webgpu_force_fallback_adapter = false,
    use_ime = true,

    -- Appearance
    initial_cols = 100,
    initial_rows = 20,
    window_decorations = "RESIZE",
    enable_tab_bar = true,
    use_fancy_tab_bar = true,
    hide_tab_bar_if_only_one_tab = false,
    tab_max_width = 16,
    enable_scroll_bar = true,
    min_scroll_bar_height = "1cell",
    window_padding = {
        left = "0.5cell",
        right = "0.5cell",
        top = "0.5cell",
        bottom = "0.5cell",
    },

    -- Visual
    font = wezterm.font("Iosevka", { weight = "Regular", italic = false }),
    font_size = 12,
    -- Overwrites `color_scheme'
    colors = riceconf_meta.rice_color_scheme("Galaxy"),
    cursor_blink_rate = 0,
    default_cursor_style = "SteadyBlock",
    window_background_opacity = 0.9,
    text_background_opacity = 1.0,

    -- Keybindings
    -- Use my own keybindings
    disable_default_key_bindings = true,
    disable_default_mouse_bindings = false,
    key_map_preference = "Mapped",
    -- leader = { mods = "CTRL", key = "`" },
    keys = rice_keys,
    key_tables = {
        search_mode = rice_search_mode_table,
        copy_mode = rice_copy_mode_table,
        rice_transient_mode_table = rice_transient_mode_table,
    },

    -- Launcher
    launch_menu = rice_launcher_menu,
}

return riceconf_meta:rice_bind(riceconf)
