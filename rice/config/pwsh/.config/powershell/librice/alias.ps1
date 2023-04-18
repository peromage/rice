### alias.ps1 -- Common aliases

### Hash calculation
function hash-md5 {
    param($file)
    (Get-FileHash -Algorithm MD5 $file).Hash
}

function hash-sha1 {
    param($file)
    (Get-FileHash -Algorithm SHA1 $file).Hash
}

function hash-sha256 {
    param($file)
    (Get-FileHash -Algorithm SHA256 $file).Hash
}

function hash-sha512 {
    param($file)
    (Get-FileHash -Algorithm SHA512 $file).Hash
}

### Authentication agents
function reload-ssh-agent {
    $Env:SSH_AUTH_SOCK = "${XDG_RUNTIME_DIR}/ssh-agent.socket"
    if (Test-Path $Env:SSH_AUTH_SOCK) {
        return
    }
    Invoke-Expression (ssh-agent -a $Env:SSH_AUTH_SOCK)
}

function reload-gpg-agent {
    param([switch]$f)
    if ($f) {
        "Restarting gpg-agent..."
        gpgconf --kill gpg-agent
    }
    Remove-Item Env:SSH_AGENT_PID -ErrorAction SilentlyContinue
    $Env:SSH_AUTH_SOCK = "$(gpgconf --list-dirs agent-ssh-socket)"
    $Env:GPG_TTY = "$(tty)"
    gpg-connect-agent updatestartuptty /bye > /dev/null
}
