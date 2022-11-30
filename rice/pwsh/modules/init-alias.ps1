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
function hash_md5 {
    param($file)
    (Get-FileHash -Algorithm MD5 $file).Hash
}

function hash_sha1 {
    param($file)
    (Get-FileHash -Algorithm SHA1 $file).Hash
}

function hash_sha256 {
    param($file)
    (Get-FileHash -Algorithm SHA256 $file).Hash
}

function hash_sha512 {
    param($file)
    (Get-FileHash -Algorithm SHA512 $file).Hash
}

### Authentication agents
function reload_ssh_agent {
    $Env:SSH_AUTH_SOCK = "${XDG_RUNTIME_DIR}/ssh-agent.socket"
    if (Test-Path $Env:SSH_AUTH_SOCK) {
        return
    }
    Invoke-Expression (ssh-agent -a $Env:SSH_AUTH_SOCK)
}

function reload_gpg_agent {
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

### Function decoration
## Behavior definitions follow Emacs "define_advice". Only function is supported
function define_advice {
    param([Parameter(Mandatory, Position=0, ParameterSetName="before")]
          [Parameter(Mandatory, Position=0, ParameterSetName="after")]
          [Parameter(Mandatory, Position=0, ParameterSetName="around")]
          [Parameter(Mandatory, Position=0, ParameterSetName="override")]
          [Parameter(Mandatory, Position=0, ParameterSetName="filter_args")]
          [Parameter(Mandatory, Position=0, ParameterSetName="filter_return")]
          [string]
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
          $filter_return
         )

    $function_funcname = "function:${funcname}"
    if (-not (Get-Item $function_funcname -ErrorAction SilentlyContinue)) {
        "$function_funcname does not exist."
        return
    }
    $original_func = (Get-Item $function_funcname).ScriptBlock

    ## Process advice
    if ($before) {
        Set-Item $function_funcname {
            &$advice @args
            &$original_func @args
        }.GetNewClosure()
        return
    }

    if ($after) {
        Set-Item $function_funcname {
            &$original_func @args
            &$advice @args
        }.GetNewClosure()
        return
    }

    if ($around) {
        Set-Item $function_funcname {
            &$advice $original_func @args
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
            $result = &$advice @args
            &$original_func @result
        }.GetNewClosure()
        return
    }

    if ($filter_return) {
        Set-Item $function_funcname {
            $result = &$original_func @args
            &$advice @result
        }.GetNewClosure()
        return
    }
}
