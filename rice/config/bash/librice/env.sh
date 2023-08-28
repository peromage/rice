### env.sh -- POSIX environment setup

## This section must be POSIX compliant
for i in "$@"; do
    case "$i" in
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
        gpg-agent)
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
done
unset i
