### init-default.ps1 -- PowerShell settings

### Environment variables
$env:PATH += [IO.Path]::PathSeparator + (Join-Path $ripwsh.home.Parent.FullName bin)
$env:EDITOR = "vim"

### PSReadLine
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
