### init-alias.ps1 -- Common aliases

### Common
function ll {
    ls -lahF --color=auto @args
}

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
