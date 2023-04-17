--- init.lua --- Bootstrap -*- lua-indent-level: 4; -*-

-- For some reasons if require() is called inside of rice_merge() it would cause
-- troubles.
local ricemeta = require "librice.meta"
local defaults = require "librice.defaults"
local keybindings = require "librice.keybindings"
local launcher = require "librice.launcher"
local events = require "librice.events"
local commands = require "librice.commands"

return ricemeta:rice_bind({}):rice_merge(
    defaults,
    keybindings,
    launcher,
    events,
    commands,
    -- Individual settings
    {
        -- default_prog = {"pwsh", "-NoLogo"}
    }
)
