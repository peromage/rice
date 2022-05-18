### init-env.ps1 -- Environment variables

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

### XDG
$env:XDG_DATA_HOME = Join-Path $HOME .local/share
$env:XDG_STATE_HOME = Join-Path $HOME .local/state
$env:XDG_CONFIG_HOME = Join-Path $HOME .config
$env:XDG_CACHE_HOME = Join-Path $HOME .cache

### PATH
$env:PATH += [IO.Path]::PathSeparator + (Join-Path $ripwsh.home.Parent.FullName bin)

### Common
$env:EDITOR = "vim"
