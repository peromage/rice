### init.ps1 -- PowerShell bootstrap

### Prerequisites
## PowerShell 7 is required
if ($PSVersionTable.PSVersion.Major -lt 7) {
    Write-Host -ForegroundColor Red "PowerShell 7 and above required"
    return
}

### Initialization
## Rice variables
$rice = @{
    home = Get-Item $PSScriptRoot
    privileged = $IsWindows ? ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator) : ((id -u) -eq 0)
    platform_windows = $IsWindows ? $true : $false
}

## Return a string with dot sourcing syntax
## Due to the limitation, sourcing has to be done in the global scope
function rice_include {
    param ($file)
    return ". $(Join-Path $rice.home.FullName $file) $args"
}

## PSReadLine
&{
    $my_psreadline_options = @{
        EditMode = "Emacs"
        HistoryNoDuplicates = $true
        HistorySearchCursorMovesToEnd = $true
        HistorySearchCaseSensitive = $false
        HistorySaveStyle = "SaveIncrementally"
        PredictionSource = "History"
    }
    Set-PSReadLineOption @my_psreadline_options
    Set-PSReadLineKeyHandler -Key Tab -Function MenuComplete
}

### Environment variables
$Env:PATH = @(
    $Env:PATH
    (Join-Path $rice.home.Parent.FullName "scripts")
    (Join-Path $HOME ".dotnet/tools")
    (Join-Path $HOME "bin")
) -join [IO.Path]::PathSeparator
$Env:EDITOR = "vim"

### Load modules
&{
    rice_include modules/init-alias.ps1
    rice_include modules/init-hack.ps1
    rice_include modules/theme-minimalist.ps1
    if ($rice.platform_windows) {
        rice_include modules/init-alias-windows.ps1
    }
} | ForEach-Object { Invoke-Expression $_ }
