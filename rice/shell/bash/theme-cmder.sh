### theme-cmder.sh -- Cmder style prompt

_RET_FACE_COLOR_() {
    case "$?" in
        0) echo -e '\e[32m:)\e[0m';;
        *) echo -e '\e[31m:(\e[0m';;
    esac
}

case "$UID" in
    0) PS1=' \[\e[31m\]\u@\h\[\e[0m\]:\[\e[33m\]\w\[\e[0m\] \[\e[37m\][\d \A]\[\e[0m\]\n $(_RET_FACE_COLOR_) +++ ';;
    *) PS1=' \[\e[32m\]\u@\h\[\e[0m\]:\[\e[33m\]\w\[\e[0m\] \[\e[37m\][\d \A]\[\e[0m\]\n $(_RET_FACE_COLOR_) --- ';;
esac
