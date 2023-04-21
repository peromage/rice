--- random_color.lua --- Global commands -*- lua-indent-level: 4; -*-

local wezterm = require "wezterm"

local get_random_color_scheme = function()
    -- Return a random color scheme from `wezterm.color.get_builtin_schemes'.
    -- Returned values are the color scheme's name and the object itself.
    local schemes = wezterm.color.get_builtin_schemes()
    local scheme_names = {}
    for k,_ in pairs(schemes) do
        table.insert(scheme_names, k)
    end
    local name = scheme_names[math.random(#scheme_names)]
    return name, schemes[name]
end

--- Randomize current color scheme.
-- Note: This is effective to all the present windows.
local randomize = function()
    local new_color_scheme_name, new_color_scheme = get_random_color_scheme()
    for _,win in ipairs(wezterm.gui.gui_windows()) do
        local overrides = win:get_config_overrides() or {}
        overrides.colors = new_color_scheme
        win:set_config_overrides(overrides)
    end
    wezterm.log_info("Color scheme: " .. new_color_scheme_name)
end

--- Module table
return { randomize = randomize }
