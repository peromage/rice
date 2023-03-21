--- defaults.lua --- Default config -*- lua-indent-level: 4; -*-

local ricemeta = require "librice.meta"
local wezterm = require "wezterm"

return ricemeta:rice_bind({
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
    colors = ricemeta.util.color_scheme("Galaxy"),
    cursor_blink_rate = 0,
    default_cursor_style = "SteadyBlock",
    window_background_opacity = 0.9,
    text_background_opacity = 1.0,
})
