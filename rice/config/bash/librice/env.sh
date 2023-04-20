### env.sh -- Selective environment variables

for i in "$@"; do
    case "$i" in
        path)
            export PATH="$PATH\
:${HOME}/bin\
:${HOME}/.dotnet/tools\
"
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
            export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"
            # [ ! -e $SSH_AUTH_SOCK ] && eval $(ssh-agent -a $SSH_AUTH_SOCK)
            ;;
        gpg-agent)
            unset SSH_AGENT_PID
            export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
            # export GPG_TTY=$(tty)
            ;;
        *)
            echo "No definition for $i"
            ;;
    esac
done
unset i
