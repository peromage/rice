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

## Environment variables
$env:PATH += [IO.Path]::PathSeparator + (Join-Path $rice.home.Parent.FullName "scripts")
$env:EDITOR = "vim"

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

### Load modules
Invoke-Expression (rice_include modules/init-alias.ps1)
Invoke-Expression (rice_include modules/theme-minimalist.ps1)
if ($rice.platform_windows) {
    Invoke-Expression (rice_include modules/init-alias-windows.ps1)
}
