#!/bin/sh

## Build an ANSI sequence.

SEPARATOR=" "

select_color() {
    case "$1" in
        :reset)    printf "\e[0m"    ;;
        :black)    printf "\e[30m"   ;;
        :red)      printf "\e[31m"   ;;
        :green)    printf "\e[32m"   ;;
        :yellow)   printf "\e[33m"   ;;
        :blue)     printf "\e[34m"   ;;
        :magenta)  printf "\e[35m"   ;;
        :cyan)     printf "\e[36m"   ;;
        :white)    printf "\e[37m"   ;;
        :black1)   printf "\e[30;1m" ;;
        :red1)     printf "\e[31;1m" ;;
        :green1)   printf "\e[32;1m" ;;
        :yellow1)  printf "\e[33;1m" ;;
        :blue1)    printf "\e[34;1m" ;;
        :magenta1) printf "\e[35;1m" ;;
        :cyan1)    printf "\e[36;1m" ;;
        :white1)   printf "\e[37;1m" ;;
        *)         printf ""         ;;
    esac
}

loop_print() {
    S="" color=""
    for i in "$@"; do
        color="$(select_color "$i")"
        if [ -n "$color" ]; then
            printf "%s" "$color"
        else
            printf "%s" "${S}${i}"
            [ -z "$S" ] && S="$SEPARATOR"
        fi
    done
    unset color S
}

loop_print "$@"
printf "%s" "$(select_color :reset)"
