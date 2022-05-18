<#
.SYNOPSIS
A simple dirty and quick script to setup my repos
#>
param ($cmd='')

function loge {
    Write-Host -ForegroundColor Red @args
}

function logw {
    Write-Host -ForegroundColor Yellow @args
}

function logi {
    Write-Host -ForegroundColor Cyan @args
}

function logd {
    Write-Host @args
}

if ($null -eq $MYREPOS) {
    logw "Environment variable MYREPOS is not set. You can set it with this format: LocalDir1[,GitURI1];LocalDir2[,GitURI2]"
    exit 1
}

$repos = @()
$idx_local = 0
$idx_git = 1
foreach ($local_bind in ($MYREPOS -split ';')) {
    if ($local_bind -match '([^,]+),?([^,]*)') {
        $repos += ,@($Matches[1], $Matches[2])
    }
}

function git_check_status_clean {
    git status --porcelain
}

function git_do_if_clean {
    param ($local_dir)
    Push-Location $local_dir
    if ($null -eq $(git_check_status_clean)) {
        git @args
    }
    else {
        git status
    }
    Pop-Location
}

function git_clone {
    param ($repo_url, $local_dir)
    if (Test-Path $local_dir) {
        loge "Directory already exists: $local_dir, skipping"
        return
    }
    git clone $repo_url $local_dir
}

$commands = @{}

$commands["init"] = {
    foreach ($repo in $repos) {
        logi "Initializing $($repo[$idx_local])"
        git_clone $repo[$idx_git] $repo[$idx_local]
    }
}

$commands["sync"] = {
    foreach ($repo in $repos) {
        logi "Syncing $($repo[$idx_local])"
        git_do_if_clean $repo[$idx_local] pull
    }
}

$commands["push"] = {
    foreach ($repo in $repos) {
        logi "Pushing $($repo[$idx_local])"
        git_do_if_clean $repo[$idx_local] push
    }
}

$commands["status"] = {
    foreach ($repo in $repos) {
        logi "Checking $($repo[$idx_local])"
        git_do_if_clean $repo[$idx_local] status
    }
}

if ($commands.ContainsKey($cmd)) {
    &$commands[$cmd] @args
} else {
    loge "${cmd}: Command not found!"
}
