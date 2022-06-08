### theme-canonical.sh -- Normal mediocre prompt

case "$UID" in
    0) PS1='\[\e[0;90m\][\[\e[0;1;31m\]\u\[\e[0;90m\]@\[\e[0;1;31m\]\h \[\e[0;1;91m\]\w\[\e[0;90m\]]\[\e[0;90m\]# \[\e[0m\]';;
    *) PS1='\[\e[0;90m\][\[\e[0;1;94m\]\u\[\e[0;90m\]@\[\e[0;1;94m\]\h \[\e[0;1;96m\]\w\[\e[0;90m\]]\[\e[0;90m\]$ \[\e[0m\]';;
esac
