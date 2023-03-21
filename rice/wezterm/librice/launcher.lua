--- launcher.lua --- Launch menu -*- lua-indent-level: 4; -*-

local ricemeta = require "librice.meta"

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

return {
    launch_menu = rice_launch_menu,
}
