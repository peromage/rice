--- init.lua --- Bootstrap -*- lua-indent-level: 4; -*-

--- Loading modules
-- For some reasons if require() is called inside of rice_merge() it would cause
-- troubles.
local ricemeta = require "librice.meta"
local defaults = require "librice.defaults"
local keybindings = require "librice.keybindings"
local launcher = require "librice.launcher"
local events = require "librice.events"
local commands = require "librice.commands"
local conf = ricemeta:rice_bind({}):rice_merge(
    defaults,
    keybindings,
    launcher,
    events,
    commands
)

--- Random customizations
conf:rice_merge(
    {
        -- default_prog = {"pwsh", "-NoLogo"}
    }
)

conf.launch_menu:rice_push(
    {
        -- label = "VM",
        -- args = { "ssh", "vm" },
    }
)

return conf
