--- meta.lua --- Meta table -*- lua-indent-level: 4; -*-

local wezterm = require "wezterm"
local utility = {}

--- Utility functions
-- Load a builtin color scheme and return the object with some personal flavors.
-- Returned value is a color scheme object.
function utility.custom_color_scheme(scheme_name)
    local scheme = wezterm.get_builtin_color_schemes()[scheme_name]
    -- Make the scrollbar more visible (lightness less than 0.6 considered as
    -- a dark theme)
    local h, s, l, a = wezterm.color.parse(scheme.background):hsla()
    scheme.scrollbar_thumb = wezterm.color.from_hsla(h, s, l < 0.6 and 1 or 0, a)
    return scheme
end

-- Return a random color scheme from `wezterm.color.get_builtin_schemes'.
-- Returned values are the color scheme's name and the object itself.
function utility.random_color_scheme()
    local schemes = wezterm.color.get_builtin_schemes()
    local scheme_names = {}
    for k,_ in pairs(schemes) do
        table.insert(scheme_names, k)
    end
    local name = scheme_names[math.random(#scheme_names)]
    return name, schemes[name]
end

-- Increment/decrement the value based on the step.
-- The returned value always falls between min_val and max_val.
-- If input value is nil , nil is returned.
-- If input value is out of range, it will be stepped from the closest boundary.
function utility.step(val, min_val, max_val, step)
    if nil == val or nil == min_val or nil == max_val or nil == step then
        return nil
    end
    -- Step the value
    if val < min_val then
        val = min_val + step
    elseif val > max_val then
        val = max_val + step
    else
        val = val + step
    end
    -- Boundary check
    if val < min_val then
        return min_val
    end
    if val > max_val then
        return max_val
    end
    return val
end

--- Some meta data
utility.platform = wezterm.target_triple == "x86_64-pc-windows-msvc" and "win" or "*nix"

--- Module table
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
            if "table" == type(tbl) then
                for k,v in pairs(tbl) do
                    self[k] = v
                end
            end
        end
        return self
    end,

    -- A utility function to push a list of elements to the end of the table in
    -- place.
    -- Return self after pushing.
    rice_push = function(self, ...)
        for _,elem in ipairs({...}) do
            table.insert(self, elem)
        end
        return self
    end,

    util = utility,
}
