### classic.ps1 -- Normal mediocre prompt

function simplify_path {
    ## Replace home prefix with "~"
    $pwd.Path -replace ([regex]::Escape($HOME)+'(.*)'),'~$1'
}

function prompt {
    if ($RICE.privileged) {
        Write-Host -NoNewline -ForegroundColor DarkGray "["
        Write-Host -NoNewline -ForegroundColor Red "$([Environment]::UserName)"
        Write-Host -NoNewline -ForegroundColor DarkGray "@"
        Write-Host -NoNewline -ForegroundColor Red "$([Environment]::MachineName)"
        Write-Host -NoNewline " "
        Write-Host -NoNewline -ForegroundColor Red "$(simplify_path)"
        Write-Host -NoNewline -ForegroundColor DarkGray "]#"
        return " "
    }
    Write-Host -NoNewline -ForegroundColor DarkGray "["
    Write-Host -NoNewline -ForegroundColor Blue "$([Environment]::UserName)"
    Write-Host -NoNewline -ForegroundColor DarkGray "@"
    Write-Host -NoNewline -ForegroundColor Blue "$([Environment]::MachineName)"
    Write-Host -NoNewline " "
    Write-Host -NoNewline -ForegroundColor Cyan "$(simplify_path)"
    Write-Host -NoNewline -ForegroundColor DarkGray "]$"
    return " "
}
