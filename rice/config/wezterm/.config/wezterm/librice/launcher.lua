--- launcher.lua --- Launch menu -*- lua-indent-level: 4; -*-

local ricemeta = require "librice.meta"

--- Custom launch menu
local rice_launch_menu = ricemeta:rice_bind({
    {
        label = "Pwsh",
        args = { "pwsh", "-NoLogo" },
    },
    {
        label = "Bash",
        args = { "bash", "-i" },
    },
})

--- Module table
return {
    launch_menu = rice_launch_menu,
}
