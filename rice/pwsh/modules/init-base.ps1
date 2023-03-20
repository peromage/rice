### init-base.ps1 -- The first script to be sourced

### Rice variables
$rice = @{
    home = Get-Item "$PSScriptRoot/.."
    privileged = $IsWindows ? ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator) : ((id -u) -eq 0)
    platform_windows = $IsWindows ? $true : $false
}

### Include function
## Return a string with dot sourcing syntax
## Due to the limitation, sourcing has to be done in the global scope
function rice_include_expr {
    param ($file)
    return ". $(Join-Path $rice.home.FullName $file) $args"
}
