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

set_color() {
    echo -ne ${COLORS[$1]}
}

echo_black() {
    set_color bright_black
    echo "$@"
    set_color reset
}

echo_red() {
    set_color bright_red
    echo "$@"
    set_color reset
}

echo_green() {
    set_color bright_green
    echo "$@"
    set_color reset
}

echo_yellow() {
    set_color bright_yellow
    echo "$@"
    set_color reset
}

echo_blue() {
    set_color bright_blue
    echo "$@"
    set_color reset
}

echo_magenta() {
    set_color bright_magenta
    echo "$@"
    set_color reset
}

echo_cyan() {
    set_color bright_cyan
    echo "$@"
    set_color reset
}

echo_white() {
    set_color bright_white
    echo "$@"
    set_color reset
}

logi() {
    set_color bright_white
    echo "[ INFO ]" "$@"
    set_color reset
}

logw() {
    set_color bright_yellow
    echo "[ WARNING ]" "$@"
    set_color reset
}

loge() {
    set_color bright_red
    echo "[ ERROR ]" "$@"
    set_color reset
}
