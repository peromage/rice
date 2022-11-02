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

### Function decoration
## Behavior definitions follow Emacs "define_advice"
## Only function is supported
function define_advice {
    param([Parameter(Mandatory, Position=0, ParameterSetName="before")]
          [Parameter(Mandatory, Position=0, ParameterSetName="after")]
          [Parameter(Mandatory, Position=0, ParameterSetName="around")]
          [Parameter(Mandatory, Position=0, ParameterSetName="override")]
          [Parameter(Mandatory, Position=0, ParameterSetName="filter_args")]
          [Parameter(Mandatory, Position=0, ParameterSetName="filter_return")]
          [Parameter(Mandatory, Position=0, ParameterSetName="restore")]
          [string]
          ## Can be a function, cmdlet or alias
          $funcname,

          [Parameter(Mandatory, Position=1, ParameterSetName="before")]
          [Parameter(Mandatory, Position=1, ParameterSetName="after")]
          [Parameter(Mandatory, Position=1, ParameterSetName="around")]
          [Parameter(Mandatory, Position=1, ParameterSetName="override")]
          [Parameter(Mandatory, Position=1, ParameterSetName="filter_args")]
          [Parameter(Mandatory, Position=1, ParameterSetName="filter_return")]
          [scriptblock]
          $advice,

          [Parameter(Mandatory, ParameterSetName="before")]
          [switch]
          $before,

          [Parameter(Mandatory, ParameterSetName="after")]
          [switch]
          $after,

          [Parameter(Mandatory, ParameterSetName="around")]
          [switch]
          $around,

          [Parameter(Mandatory, ParameterSetName="override")]
          [switch]
          $override,

          [Parameter(Mandatory, ParameterSetName="filter_args")]
          [switch]
          $filter_args,

          [Parameter(Mandatory, ParameterSetName="filter_return")]
          [switch]
          $filter_return,

          [Parameter(Mandatory, ParameterSetName="restore")]
          [switch]
          $restore
         )

    $function_funcname = "function:${funcname}"

    if (-not (Get-Item $function_funcname -ErrorAction SilentlyContinue)) {
        "$function_funcname does not exist."
        return
    }

    $saved_funcname = "saved_${funcname}"
    $function_saved_funcname = "function:${saved_funcname}"

    ## Check restoration before proceeding advice
    if ($restore) {
        if (-not (Get-Item $function_saved_funcname -ErrorAction SilentlyContinue)) {
            "No advice defined for $funcname"
            return
        }
        Copy-Item $function_saved_funcname $function_funcname
        Remove-Item $function_saved_funcname
        return
    }

    ## Backup old function definition
    Invoke-Expression "function global:${saved_funcname} {}"
    Copy-Item $function_funcname $function_saved_funcname

    ## Process advice
    if ($before) {
        Set-Item $function_funcname {
            &$advice @args
            &$saved_funcname @args
        }.GetNewClosure()
        return
    }

    if ($after) {
        Set-Item $function_funcname {
            &$saved_funcname @args
            &$advice @args
        }.GetNewClosure()
        return
    }

    if ($around) {
        Set-Item $function_funcname {
            &$advice (Get-Item $function_saved_funcname) @args
        }.GetNewClosure()
        return
    }

    if ($override) {
        Set-Item $function_funcname {
            &$advice @args
        }.GetNewClosure()
        return
    }

    if ($filter_args) {
        Set-Item $function_funcname {
            &$saved_funcname (&$advice @args)
        }.GetNewClosure()
        return
    }

    if ($filter_return) {
        Set-Item $function_funcname {
            &$advice (&$saved_funcname @args)
        }.GetNewClosure()
        return
    }
}
