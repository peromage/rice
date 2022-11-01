### theme-minimalist.ps1 -- My pwsh prompt. It is lite

function simplify_path {
    ## Replace home prefix with "~"
    $pwd.Path -replace ([regex]::Escape($HOME)+'(.*)'),'~$1'
}

function prompt {
    if ($rice.privileged) {
        Write-Host -NoNewline -ForegroundColor Red "$(simplify_path)"
        Write-Host -NoNewline -ForegroundColor DarkGray "!>"
        return " "
    }
    Write-Host -NoNewline -ForegroundColor Cyan "$(simplify_path)"
    Write-Host -NoNewline -ForegroundColor DarkGray ">"
    return " "
}
