### theme-minimalist.ps1 -- My pwsh prompt. It is lite

function global:_simplifyHomePath {
    $pwd.Path -replace ([regex]::Escape($HOME)+'(.*)'),'~$1'
}

if ($ripwsh.root) {
function global:prompt {
    Write-Host -NoNewline -ForegroundColor Red "$(_simplifyHomePath)"
    Write-Host -NoNewline -ForegroundColor DarkGray "!>"
    return " "
}} else {
function global:prompt {
    Write-Host -NoNewline -ForegroundColor Cyan "$(_simplifyHomePath)"
    Write-Host -NoNewline -ForegroundColor DarkGray ">"
    return " "
 }}
