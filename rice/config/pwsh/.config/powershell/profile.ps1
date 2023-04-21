### profile.ps1  -- Pwsh init -*- outline-regexp: "###\\(#* \\)"; -*-

### Prerequisites
## PowerShell 7 is required
if ($PSVersionTable.PSVersion.Major -lt 7) {
    Write-Host -ForegroundColor Red "PowerShell 7 and above required"
    return
}

### Environment variables
$RICE = @{}
$RICE.rc = Get-Item "$PSScriptRoot"
$RICE.custom_rc = (Join-Path $RICE.rc "custom.ps1")
$RICE.privileged = $IsWindows ? ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator) : ((id -u) -eq 0)

### PSReadLine and prompt
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

### Commands
function rice_include_expr {
    <# Return a string with dot sourcing syntax.
Due to the limitation, sourcing has to be done in the global scope. e.g.
  iex (rice_include_expr file) #>
    param ($file)
    return ". $(Join-Path $RICE.rc.FullName $file) $args"
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

### Random stuff
if (Test-Path -Type Leaf $RICE.custom_rc) { . $RICE.custom_rc }
