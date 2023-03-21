### init.ps1 -- PowerShell bootstrap

### Prerequisites
## PowerShell 7 is required
if ($PSVersionTable.PSVersion.Major -lt 7) {
    Write-Host -ForegroundColor Red "PowerShell 7 and above required"
    return
}

### Initialization
. $PSScriptRoot/librice/base.ps1

### PSReadLine
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
    rice_include_expr librice/alias.ps1
    rice_include_expr librice/hack.ps1
    rice_include_expr theme/minimalist.ps1
    if ($rice.platform_windows) {
        rice_include_expr librice/alias-windows.ps1
    }
} | ForEach-Object { Invoke-Expression $_ }
