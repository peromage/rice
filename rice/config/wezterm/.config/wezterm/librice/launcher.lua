--- launcher.lua --- Launch menu -*- lua-indent-level: 4; -*-

local ricemeta = require "librice.meta"

--- Custom launch menu
local rice_launch_menu = ricemeta:rice_bind({
    {
        label = "Bash",
        args = { "bash", "-i" },
    },
    {
        label = "Fish",
        args = { "fish", "-i" },
    },
    {
        label = "Pwsh",
        args = { "pwsh", "-NoLogo" },
    },
})

--- Module table
return {
    launch_menu = rice_launch_menu,
}
