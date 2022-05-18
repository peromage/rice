### init-cmd-win.sh -- Commands for Windows

[[ $OS =~ [Ww]indows ]] && {

## MSYS
alias msys2-update="pacman --needed -S bash pacman pacman-mirrors msys2-runtime"

## Cygwin
alias cygwin-install="cygwin-setup --no-admin --no-shortcuts"

}
