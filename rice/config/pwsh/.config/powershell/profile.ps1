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
$RICE.rc = Get-Item "$PSScriptRoot"
$RICE.custom_rc = (Join-Path $RICE.rc "custom.ps1")
$RICE.privileged = $IsWindows ? ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator) : ((id -u) -eq 0)
## Modules will be autoloaded
$Env:PSModulePath += [IO.Path]::PathSeparator + (Join-Path $RICE.rc librice)

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
    return (Join-Path $RICE.rc.FullName "librice" "${name}.ps1")
}

function simplify_home_path {
    <# Replace home prefix with "~" #>
    $pwd.Path -replace ([regex]::Escape($HOME)+'(.*)'),'~$1'
}

## Windows specific
if ($IsWindows) {
    function test_admin {
        return (
            [Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()
        ).IsInRole(
            [Security.Principal.WindowsBuiltinRole]::Administrator
        )
    }

    function run_as_admin {
        if (test_admin) {
            "Current session is running with admin privilege already."
            return
        }
        $proc = New-Object -TypeName System.Diagnostics.Process
        $proc.StartInfo.FileName = "${PSHOME}/pwsh.exe"
        $proc.StartInfo.Arguments = $args.Count -gt 0 ? "-command cd $pwd; $($args -join ' ')" : "-noexit -command cd $pwd"
        $proc.StartInfo.UseShellExecute = $true
        $proc.StartInfo.Verb = "runas"
        $proc.Start() | Out-Null
    }

    function set_environment_variable {
        <# Exporting variables #>
        param($var, $value)
        Set-Item -Force -Path "Env:$var" -Value $value
    }

    function kill_process {
        <# Terminate a process #>
        param($proc)
        Get-Process $proc | Stop-Process
    }

    function create_file {
        <# Create empty files #>
        if ($args.Count -lt 1) {
            "No file name specified."
            return
        }
        $args | ForEach-Object {
            Out-File -FilePath $_
        }
    }

    ## POSIX-like commands
    Set-Alias export set_environment_variable
    Set-Alias pkill kill_process
    Set-Alias touch create_file
    Set-Alias sudo run_as_admin
    Set-Alias su run_as_admin
    Set-Alias ll Get-ChildItem
    Set-Alias which Get-Command
    Set-Alias df Get-Volume
    Set-Alias grep Select-String
}

### Random stuff ###############################################################
if (Test-Path -Type Leaf $RICE.custom_rc) { . $RICE.custom_rc }
