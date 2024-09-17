### minimal-prompt.psm1 --- Minimal prompt style

$isPrivileged = $IsWindows ? ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator) : ((id -u) -eq 0)

function simplify_home_path {
    <# Replace home prefix with "~" #>
    $pwd.Path -replace ([regex]::Escape($HOME)+'(.*)'),'~$1'
}

$function:prompt = {
    if ($isPrivileged) {
        Write-Host -NoNewline -ForegroundColor Red "$(simplify_home_path)"
        Write-Host -NoNewline -ForegroundColor DarkGray "!>"
        return " "
    }
    Write-Host -NoNewline -ForegroundColor Cyan "$(simplify_home_path)"
    Write-Host -NoNewline -ForegroundColor DarkGray ">"
    return " "
}

Export-ModuleMember -Function prompt
