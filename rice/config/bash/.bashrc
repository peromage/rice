### .bashrc  -- Bash init -*- outline-regexp: "###\\(#* [^ \t\n]\\)"; -*-

### Environment ################################################################
## This section must be POSIX compliant since it can be sourced by any shell
rice_env() {
    case "$1" in
        path)
            export PATH="\
${HOME}/bin:\
${HOME}/.local/bin:\
$PATH"
            ;;
        path-dotnet)
            export PATH="${HOME}/.dotnet/tools:${PATH}"
            ;;
        xdg)
            export XDG_DATA_HOME="$HOME/.local/share"
            export XDG_STATE_HOME="$HOME/.local/state"
            export XDG_CONFIG_HOME="$HOME/.config"
            export XDG_CACHE_HOME="$HOME/.cache"
            ;;
        fcitx)
            export GTK_IM_MODULE="fcitx"
            export QT_IM_MODULE="fcitx"
            export XMODIFIERS="@im=fcitx"
            ;;
        firefox-wayland)
            export MOZ_ENABLE_WAYLAND=1
            ;;
        firefox-x11)
            ## Fix touchpad kinetic scrolling
            export MOZ_USE_XINPUT2=1
            ;;
        editor-vim)
            export EDITOR="vim"
            ;;
        editor-mg)
            export EDITOR="mg"
            ;;
        shell-bash)
            export SHELL="/usr/bin/bash"
            ;;
        shell-fish)
            export SHELL="/usr/bin/fish"
            ;;
        shell-pwsh)
            export SHELL="/usr/bin/pwsh"
            ;;
        ssh-agent)
            unset SSH_AGENT_PID
            SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)" && export SSH_AUTH_SOCK
            ;;
        prompt-classic)
            case "$(id -u)" in
                0) PS1='\[\e[1;30m\][\[\e[0;1;31m\]\u\[\e[1;30m\]@\[\e[0;1;31m\]\h \[\e[0;1;31m\]\w\[\e[1;30m\]]#\[\e[0m\] ';;
                *) PS1='\[\e[1;30m\][\[\e[0;1;34m\]\u\[\e[1;30m\]@\[\e[0;1;34m\]\h \[\e[0;1;36m\]\w\[\e[1;30m\]]$\[\e[0m\] ';;
            esac
            ;;
        *)
            echo "No env definition for $i"
            ;;
    esac
}

case "$1" in
    -e|--env)
        shift
        for i in "$@"; do rice_env "$i"; done; unset i
        return 0
esac

## End Environment

### Bash specific ##############################################################
## Source guard
[[ -z $BASH_VERSION ]] && return 1
## Interactive mode only
[[ ! "$-" =~ "i" ]] && return 2
## Emacs TRAMP mode
[[ "$TERM" =~ [Dd]umb ]] && PS1="$ " && return 3

### Environment variables ######################################################
declare -A RICE
RICE[rc]="$(dirname "$(realpath -s "${BASH_SOURCE[0]}")")" ## where this script is (no follow)
RICE[custom_rc]="${RICE[rc]}/custom.bash"
RICE[os_windows]=$([[ "$OS" =~ [Ww]indows ]] && echo 1)

## Presets
for i in prompt-classic path ssh-agent
do rice_env $i; done; unset i

### Commands ###################################################################
function rice_include {
    ## Source a .bash script file under librice directory.
    ## The name should be the file basename without extension .bash.
    ## Usage: rice_include name [args]
    local name="${1:-}"; shift
    source "${RICE[rc]}/librice/${name}.bash" "$@";
}

function string_join {
    ## Usage: srting-join delimiter string1 string2 ...
    local d="${1:-}" f="${2:-}"
    if shift 2; then
        printf "%s" "$f" "${@/#/$d}"
    fi
}

### Random stuff ###############################################################
[[ -e "${RICE[custom_rc]}" ]] && source "${RICE[custom_rc]}"
