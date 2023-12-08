### win-to-unix.psm1 --- Unix like command mapping for Windows

if (-not $IsWindows) {
    return
}

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

Export-ModuleMember -Alias * -Function *
