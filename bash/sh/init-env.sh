### init-env.sh -- Environment variables

### XDG
export XDG_DATA_HOME=$HOME/.local/share
export XDG_STATE_HOME=$HOME/.local/state
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache

### Input method
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

### PATH
PATH=$PATH:$(dirname $ribash_home)/bin

### Common
export EDITOR=vim
