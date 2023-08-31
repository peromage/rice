### profile.ps1  -- Pwsh init -*- outline-regexp: "###\\(#* [^ \t\n]\\)"; -*-

### Prerequisites ##############################################################
## PowerShell 7 is required
if ($PSVersionTable.PSVersion.Major -lt 7) {
    Write-Host -ForegroundColor Red "PowerShell 7 and above required"
    return
}

### Pwsh config ################################################################
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

### Environment variables ######################################################
$RICE = @{}
$RICE.root_dir = Get-Item "$PSScriptRoot"
$RICE.custom_rc = (Join-Path $RICE.root_dir "custom.ps1")
$RICE.privileged = $IsWindows ? ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator) : ((id -u) -eq 0)
## Modules will be autoloaded
$Env:PSModulePath += [IO.Path]::PathSeparator + (Join-Path $RICE.root_dir librice)

$function:prompt = {
    if ($RICE.privileged) {
        Write-Host -NoNewline -ForegroundColor Red "$(simplify_home_path)"
        Write-Host -NoNewline -ForegroundColor DarkGray "!>"
        return " "
    }
    Write-Host -NoNewline -ForegroundColor Cyan "$(simplify_home_path)"
    Write-Host -NoNewline -ForegroundColor DarkGray ">"
    return " "
}

### Commands ###################################################################
function rice_include {
    <# Return a string of a .ps1 script under librice directory.
The name should be the file basename without extension .ps1.
Due to the limitation, sourcing has to be done in the global scope. e.g.
  . (rice_include file) [args] #>
    param ($name)
    return (Join-Path $RICE.root_dir.FullName "librice" "${name}.ps1")
}

function simplify_home_path {
    <# Replace home prefix with "~" #>
    $pwd.Path -replace ([regex]::Escape($HOME)+'(.*)'),'~$1'
}

## Windows specific
Import-Module win-to-unix

### Random stuff ###############################################################
if (Test-Path -Type Leaf $RICE.custom_rc) { . $RICE.custom_rc }
