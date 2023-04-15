#!/bin/bash

## Build an ANSI sequence.

for i in "$@"; do
    case "$i" in
        :reset)     echo -n "\e[0m"     ;;
        :black)     echo -n "\e[30m"    ;;
        :red)       echo -n "\e[31m"    ;;
        :green)     echo -n "\e[32m"    ;;
        :yellow)    echo -n "\e[33m"    ;;
        :blue)      echo -n "\e[34m"    ;;
        :magenta)   echo -n "\e[35m"    ;;
        :cyan)      echo -n "\e[36m"    ;;
        :white)     echo -n "\e[37m"    ;;
        :black1)    echo -n "\e[30;1m"  ;;
        :red1)      echo -n "\e[31;1m"  ;;
        :green1)    echo -n "\e[32;1m"  ;;
        :yellow1)   echo -n "\e[33;1m"  ;;
        :blue1)     echo -n "\e[34;1m"  ;;
        :magenta1)  echo -n "\e[35;1m"  ;;
        :cyan1)     echo -n "\e[36;1m"  ;;
        :white1)    echo -n "\e[37;1m"  ;;
        *)          echo -n "$i"        ;;
    esac
done
