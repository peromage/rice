### This should be sourced by other scripts.

declare -A COLORS=(
    "reset"           "\e[0m"
    "black"           "\e[30m"
    "red"             "\e[31m"
    "green"           "\e[32m"
    "yellow"          "\e[33m"
    "blue"            "\e[34m"
    "magenta"         "\e[35m"
    "cyan"            "\e[36m"
    "white"           "\e[37m"
    "bright_black"    "\e[30;1m"
    "bright_red"      "\e[31;1m"
    "bright_green"    "\e[32;1m"
    "bright_yellow"   "\e[33;1m"
    "bright_blue"     "\e[34;1m"
    "bright_magenta"  "\e[35;1m"
    "bright_cyan"     "\e[36;1m"
    "bright_white"    "\e[37;1m"
)

echo_color() {
    echo -ne ${COLORS[$1]}
}

logi() {
    echo_color bright_white
    echo "[ INFO ]" "$@"
    echo_color reset
}

loge() {
    echo_color bright_red
    echo "[ ERROR ]" "$@"
    echo_color reset
}
