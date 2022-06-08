### env.sh -- Selective environment variables

for _sel in "$@"; do
case "$_sel" in
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
*)
    echo "Nothing happened"
    ;;
esac
done
unset _sel
