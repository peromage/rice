--- init.lua --- Bootstrap -*- lua-indent-level: 4; -*-

local ricemeta = require "librice.meta"
local defaults = require "librice.defaults"
local keybindings = require "librice.keybindings"
local launcher = require "librice.launcher"
local events = require "librice.events"
local commands = require "librice.commands"

-- Currently it's a bit quirky that if use require() inside of the function call
-- below Lua would complain require() doesn't return a table.
return ricemeta:rice_bind({}):rice_merge(
    defaults,
    keybindings,
    launcher,
    events,
    commands
)
