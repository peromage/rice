### profile.ps1  -- Pwsh init -*- outline-regexp: "###\\(#* [^ \t\n]\\)"; -*-

### Prerequisites
## PowerShell 7 is required
if ($PSVersionTable.PSVersion.Major -lt 7) {
    Write-Host -ForegroundColor Red "PowerShell 7 and above required"
    return
}

### Initialization
## Global variables
$MYENV = @{}
$MYENV.root_dir = Get-Item "$PSScriptRoot"
$MYENV.custom = (Join-Path $MYENV.root_dir "profile-custom.ps1")

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
$Env:PSModulePath += [IO.Path]::PathSeparator + (Join-Path $MYENV.root_dir mymodules)
Import-Module minimal-prompt
Import-Module win-to-unix

## Random stuff
if (Test-Path -Type Leaf $MYENV.custom) { . $MYENV.custom }
