--- init.lua --- Bootstrap -*- lua-indent-level: 4; -*-

--- Loading modules
local ricemeta = require "librice.meta"
local conf = ricemeta:rice_bind({}):rice_merge(
    require "librice.defaults",
    require "librice.keybindings",
    require "librice.launcher",
    require "librice.events",
    require "librice.commands"
)

--- Random stuff starts here
local ok, m = pcall(require, "custom")
if ok then m.customize(conf) end

return conf
