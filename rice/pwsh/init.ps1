### init.ps1 -- PowerShell bootstrap

### Prerequisites
## PowerShell 7 is required
if ($PSVersionTable.PSVersion.Major -lt 7) {
    Write-Host -ForegroundColor Red "PowerShell 7 and above required"
    return
}

### Initialization
$global:rice = @{
    home = Get-Item $PSScriptRoot
    root = $IsWindows ? ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator) : ((id -u) -eq 0)
    os_windows = $IsWindows ? $true : $false
}

### Source other modules relative to this file.
## Return a string with dot sourcing syntax.
function rice_include {
    param ($file, $noerror=$null)
    $file_full_path = Join-Path $rice.home.FullName $file
    if (Test-Path -PathType Leaf $file_full_path) {
        return ". ${file_full_path}"
    }
    if ($noerror) {
        return " "
    }
    return ". ${file_full_path}"
}

### Load modules
Invoke-Expression (rice_include modules/init-default.ps1)
Invoke-Expression (rice_include modules/init-alias.ps1)
Invoke-Expression (rice_include modules/init-alias-win.ps1)
Invoke-Expression (rice_include modules/theme-minimalist.ps1)
