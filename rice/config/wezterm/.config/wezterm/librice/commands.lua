--- commands.lua --- Global commands -*- lua-indent-level: 4; -*-

local ricemeta = require "librice.meta"
local wezterm = require "wezterm"

--- Randomize current color scheme.
-- Note: This is effective to all the present windows.
function rice_randomize_colors()
    local new_color_name, _ = ricemeta.util.random_color_scheme()
    local new_color = ricemeta.util.custom_color_scheme(new_color_name)
    for _,win in ipairs(wezterm.gui.gui_windows()) do
        local overrides = win:get_config_overrides() or {}
        overrides.colors = new_color
        win:set_config_overrides(overrides)
    end
    wezterm.log_info("New colors: " .. new_color_name)
end

--- Module table
return {}
