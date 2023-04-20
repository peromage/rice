### minimalist.ps1 -- My pwsh prompt. It is lite

function _prompt_simplify_path {
    ## Replace home prefix with "~"
    $pwd.Path -replace ([regex]::Escape($HOME)+'(.*)'),'~$1'
}

$_prompt_is_root = $IsWindows ? ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator) : ((id -u) -eq 0)

function prompt {
    if ($_prompt_is_root) {
        Write-Host -NoNewline -ForegroundColor Red "$(_prompt_simplify_path)"
        Write-Host -NoNewline -ForegroundColor DarkGray "!>"
        return " "
    }
    Write-Host -NoNewline -ForegroundColor Cyan "$(_prompt_simplify_path)"
    Write-Host -NoNewline -ForegroundColor DarkGray ">"
    return " "
}
