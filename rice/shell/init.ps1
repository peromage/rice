### init.ps1 -- PowerShell bootstrap

### Prerequisites
## PowerShell 7 is required
if ($PSVersionTable.PSVersion.Major -lt 7) {
    Write-Host -ForegroundColor Red "PowerShell 7 and above required"
    return
}

### Initialization
$global:ripwsh = @{
    home = Get-Item $PSScriptRoot
    root = $IsWindows ? ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator) : ((id -u) -eq 0)
}

function rinclude {
    <# Return a string which can be evaluated to source the module file.
The parameter FILE implies suffix .ps1. #>
    param ($file, $noerror=$null)
    $file = Join-Path $ripwsh.home.FullName "${file}.ps1"
    if ($noerror -and (-not (Test-Path -PathType Leaf $file))) {
        return " "
    }
    return ". ${file}"
}

### Load modules
Invoke-Expression (rinclude pwsh/init-default)
Invoke-Expression (rinclude pwsh/init-alias)
Invoke-Expression (rinclude pwsh/init-alias-win)
Invoke-Expression (rinclude pwsh/theme-minimalist)
