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
function ef { emacs -Q -nw --eval "(progn (xterm-mouse-mode 1) (dired `"$(Get-Location)`"))" }

### Authentication agents
function update_ssh_agent {
    $Env:SSH_AUTH_SOCK = "${XDG_RUNTIME_DIR}/ssh-agent.socket"
    if (Test-Path $Env:SSH_AUTH_SOCK) {
        return
    }
    Invoke-Expression (ssh-agent -a $Env:SSH_AUTH_SOCK)
}

function update_gpg_agent {
    param([switch]$f)
    if ($f) {
        "Restarting gpg-agent..."
        gpgconf --kill gpg-agent
    }
    Remove-Item Env:SSH_AGENT_PID
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
