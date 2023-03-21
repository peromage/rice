--- ricemeta.lua --- Meta table -*- lua-indent-level: 4; tab-width: 4; -*-

local wezterm = require "wezterm"

-- Since the config for Wezterm cannot have any function property, use meta table
-- to provide some additional functionalities.
return {
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
