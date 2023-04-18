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
        gpg-agent)
            export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
            export GPG_TTY="$(tty)"
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
        *)
            echo "No definition for $i"
            ;;
    esac
done
unset i
