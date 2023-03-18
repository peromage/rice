local wezterm = require "wezterm"
local act = wezterm.action

local my_keys = {
   -- Active all the time
   { mods = "CTRL",        key = "Tab",    action = act.ActivateTabRelative(1) },
   { mods = "CTRL|SHIFT",  key = "Tab",    action = act.ActivateTabRelative(-1) },
   { mods = "CTRL|SHIFT",  key = "C",      action = act.CopyTo "Clipboard" },
   { mods = "CTRL|SHIFT",  key = "V",      action = act.PasteFrom "Clipboard" },
   { mods = "CTRL|SHIFT",  key = "P",      action = act.ActivateCommandPalette },
   { mods = "CTRL|SHIFT",  key = "U",      action = act.CharSelect{ copy_on_select = true, copy_to = "ClipboardAndPrimarySelection" }},
   { mods = "CTRL|SHIFT",  key = "L",      action = act.ShowLauncher },
   { mods = "ALT",         key = "Enter",  action = act.ToggleFullScreen },
   -- Mode shift
   { mods = "LEADER",      key = "Space",  action = act.ActivateCopyMode },
   { mods = "LEADER",      key = "g",      action = act.ActivateKeyTable{ name = "my_transient_mode_table", one_shot = false }},
   { mods = "LEADER",      key = "f",      action = act.Search "CurrentSelectionOrEmptyString" },
   { mods = "LEADER",      key = "d",      action = act.ShowDebugOverlay },
   { mods = "LEADER",      key = "s",      action = act.QuickSelect },
   -- Tabs
   { mods = "LEADER",      key = "t",      action = act.SpawnTab "CurrentPaneDomain" },
   { mods = "LEADER",      key = "Q",      action = act.CloseCurrentTab{ confirm = true }},
   -- Panes
   { mods = "LEADER",      key = "2",      action = act.SplitVertical{ domain = "CurrentPaneDomain" }},
   { mods = "LEADER",      key = "3",      action = act.SplitHorizontal{ domain = "CurrentPaneDomain" }},
   { mods = "LEADER",      key = "z",      action = act.TogglePaneZoomState },
   { mods = "LEADER",      key = "o",      action = act.PaneSelect{ mode =  "Activate" } },
   { mods = "LEADER",      key = "q",      action = act.CloseCurrentPane{ confirm = true }},
   -- Windows
   { mods = "LEADER",      key = "N",      action = act.SpawnWindow },
   -- Workspace
   { mods = "LEADER",      key = "n",      action = act.SwitchWorkspaceRelative(1) },
   { mods = "LEADER",      key = "p",      action = act.SwitchWorkspaceRelative(-1) },
}

local my_transient_mode_table = {
   { mods = "CTRL",  key = "0",       action = act.ResetFontSize },
   { mods = "CTRL",  key = "=",       action = act.IncreaseFontSize },
   { mods = "CTRL",  key = "-",       action = act.DecreaseFontSize },
   { mods = "CTRL",  key = "h",       action = act.AdjustPaneSize{ "Left", 1}},
   { mods = "CTRL",  key = "j",       action = act.AdjustPaneSize{ "Down", 1}},
   { mods = "CTRL",  key = "k",       action = act.AdjustPaneSize{ "Up", 1}},
   { mods = "CTRL",  key = "l",       action = act.AdjustPaneSize{ "Right", 1}},
   { mods = "CTRL",  key = "o",       action = act.RotatePanes "Clockwise" },
   { mods = "NONE",  key = "Escape",  action = act.PopKeyTable },
   { mods = "CTRL",  key = "g",       action = act.PopKeyTable },
}

local my_copy_mode_table = {
   -- Exit
   { mods = "NONE",   key = "q",       action = act.CopyMode "Close" },
   -- Cursor movement
   { mods = "NONE",   key = "h",       action = act.CopyMode "MoveLeft" },
   { mods = "NONE",   key = "j",       action = act.CopyMode "MoveDown" },
   { mods = "NONE",   key = "k",       action = act.CopyMode "MoveUp" },
   { mods = "NONE",   key = "l",       action = act.CopyMode "MoveRight" },
   { mods = "NONE",   key = "w",       action = act.CopyMode "MoveForwardWord" },
   { mods = "NONE",   key = "e",       action = act.CopyMode "MoveForwardWordEnd" },
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
   { mods = "NONE" ,  key = "Escape",  action = act.CopyMode "ClearSelectionMode" },
   { mods = "CTRL" ,  key = "g",       action = act.CopyMode "ClearSelectionMode" },

   -- Search
   { mods = "NONE",   key = "f",       action = act.CopyMode{ JumpForward = { prev_char = false }}},
   { mods = "SHIFT",  key = "F",       action = act.CopyMode{ JumpBackward = { prev_char = false }}},
   { mods = "NONE",   key = "t",       action = act.CopyMode{ JumpForward = { prev_char = true }}},
   { mods = "SHIFT",  key = "T",       action = act.CopyMode{ JumpBackward = { prev_char = true }}},
   { mods = "NONE",   key = "/",       action = act.Search "CurrentSelectionOrEmptyString" },
   { mods = "SHIFT",  key = "#",       action = act.CopyMode "ClearPattern" },
   { mods = "NONE",   key = "n",       action = act.CopyMode "NextMatch" },
   { mods = "SHIFT",  key = "N",       action = act.CopyMode "PriorMatch" },
   { mods = "NONE",   key = ",",       action = act.CopyMode "JumpReverse" },
   { mods = "NONE",   key = ";",       action = act.CopyMode "JumpAgain" },
}

local my_search_mode_table = {
   { mods = "NONE",   key = "Escape",  action = act.CopyMode "Close" },
   { mods = "CTRL",   key = "g",       action = act.CopyMode "Close" },
   { mods = "NONE",   key = "Enter",   action = act.ActivateCopyMode },
   { mods = "CTRL",   key = "i",       action = act.CopyMode "CycleMatchType" },
   { mods = "CTRL",   key = "s",       action = act.CopyMode "NextMatch" },
   { mods = "CTRL",   key = "r",       action = act.CopyMode "PriorMatch" },
   { mods = "CTRL",   key = "k",       action = act.CopyMode "ClearPattern" },
}

return {
   -- Window appearance
   enable_tab_bar = true,
   use_fancy_tab_bar = true,
   hide_tab_bar_if_only_one_tab = false,
   tab_max_width = 16,

   -- Theme
   color_scheme = "Darcula (base16)",
   font = wezterm.font("Iosevka", { weight = "Regular", italic = false }),
   font_size = 12,

   -- Keybindings
   -- Use my own keybindings
   disable_default_key_bindings = true,
   key_map_preference = "Mapped",
   leader = { mods = "CTRL", key = "`" },
   keys = my_keys,
   key_tables = {
      search_mode = my_search_mode_table,
      copy_mode = my_copy_mode_table,
      my_transient_mode_table = my_transient_mode_table,
   },
}
