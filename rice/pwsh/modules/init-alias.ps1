### init-alias.ps1 -- Common aliases

### Common
function ll { ls -lahF --color=auto @args }

### Emacs
## Open files in the terminal
function em { emacsclient -c -nw @args }
## Open files in the current frame
function emm { emacsclient -c -n @args }
## Daemon
function emdaemon { emacsclient -e "t" 2>$null || emacs --daemon }
## Quickly open
function emq { emacs -Q }
## Dired
function ef { emacs -Q -nw --eval "(progn (xterm-mouse-mode 1) (dired \`"$(Get-Location)\`"))" }
## Quick evaluation
function ee { emacs -Q --batch --eval "(message \`"%s\`" $args)" }

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

### Linux brew
function brewenv {
    Set-Alias -Name brew -Value /home/linuxbrew/.linuxbrew/bin/brew -Scope global
    function global:_prompt_old {}
    Copy-Item function:prompt function:_prompt_old
    function global:prompt {
        Write-Host -NoNewline "(brew) "
        _prompt_old
    }
    Invoke-Expression "$(brew shellenv)"
}

function brew {
    env HOMEBREW_NO_AUTO_UPDATE=1 PATH=/home/linuxbrew/.linuxbrew/bin:$Env:PATH /home/linuxbrew/.linuxbrew/bin/brew @args
}
