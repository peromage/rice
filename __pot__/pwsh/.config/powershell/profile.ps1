### profile.ps1  -- Pwsh init -*- outline-regexp: "###\\(#* [^ \t\n]\\)"; -*-

### Prerequisites
## PowerShell 7 is required
if ($PSVersionTable.PSVersion.Major -lt 7) {
    Write-Host -ForegroundColor Red "PowerShell 7 and above required"
    return
}

### Initialization
## Global variables
$RICE = @{}
$RICE.root_dir = Get-Item "$PSScriptRoot"
$RICE.overlay = (Join-Path $RICE.root_dir "profile-overlay.ps1")

### Set it up
## Readline settings
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

## Modules
$Env:PSModulePath += [IO.Path]::PathSeparator + (Join-Path $RICE.root_dir librice)
Import-Module my-prompt
Import-Module win-to-unix

## Random stuff
if (Test-Path -Type Leaf $RICE.overlay) { . $RICE.overlay }
