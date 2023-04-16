### alias-windows.ps1 -- Utilities for Windows Only

### Privileged operations
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
Set-Alias sudo run_as_admin
Set-Alias su run_as_admin

### Exporting variables
function set_environment_variable {
    param($var, $value)
    Set-Item -Force -Path "Env:$var" -Value $value
}
Set-Alias export set_environment_variable

### Terminate a process
function kill_process {
    param($proc)
    Get-Process $proc | Stop-Process
}
Set-Alias pkill kill_process

### Create empty files
function create_file {
    if ($args.Count -lt 1) {
        "No file name specified."
        return
    }
    $args | ForEach-Object {
        Out-File -FilePath $_
    }
}
Set-Alias touch create_file

### Other POSIX-like commands
Set-Alias ll Get-ChildItem
Set-Alias which Get-Command
Set-Alias df Get-Volume
Set-Alias grep Select-String

### Cygwin
function cygwin_install {
    cygwin --no-admin --no-shortcuts @args
}
