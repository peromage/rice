--- events.lua --- Custom events -*- lua-indent-level: 4; -*-

local ricemeta = require "librice.meta"
local wezterm = require "wezterm"

--- Help functions
local function adjust_opacity(overrides, step)
    overrides.window_background_opacity = ricemeta.util.step(not overrides.window_background_opacity and 1.0 or overrides.window_background_opacity, 0.1, 1.0, step)
    return overrides
end

--- Window opacity change
wezterm.on("rice-increase-opacity", function(window, pane)
    window:set_config_overrides(adjust_opacity(window:get_config_overrides() or {}, 0.1))
end)

wezterm.on("rice-decrease-opacity", function(window, pane)
    window:set_config_overrides(adjust_opacity(window:get_config_overrides() or {}, -0.1))
end)

--- Module table
return {}
