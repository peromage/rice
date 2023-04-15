################################################################################
#
# Test color support for the shell
#
# Created by peromage 2021/02/24
# Last modified 2021/02/24
#
################################################################################

width=7
word="gYm"

# print columns with background colors
# usage: pcbg <word> <color_code>
pcbg() {
    printf "%${width}s " ${2:3}
    printf "$2%${width}s " $1
    for i in {40..47}; do
        printf "$2\e[${i}m%${width}s\e[0m " $1
    done
    printf "\n"
}

# header
printf "%${width}s %${width}s " "code" "m"
for i in {40..47}; do
    printf "%${width}s " "${i}m"
done
printf "\n"
# print row by row
pcbg $word "\e[0m"
pcbg $word "\e[1m"
for j in {30..37}; do
     pcbg $word "\e[${j}m"
     pcbg $word "\e[1;${j}m"
done

unset pcbg
unset width
unset word
