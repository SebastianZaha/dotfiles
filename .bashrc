#!/bin/sh

# Some aliases

alias ..='cd ..'
alias ...='cd ../../../'
alias ....='cd ../../../../'

alias ll="ls -l -h -a -G"

alias cutw="cut -d ' ' -f"

alias pg="ps ax | grep"
alias grep="grep --color=auto"

alias less="less -r"
alias e="emacs -nw"
alias m="mine"

alias gst="git status"
alias gco="git commit -a -m"
alias gpu="git push"
alias gpm="git push origin master"

alias qlf='qlmanage -p "$@" >& /dev/null'

alias b="bundle exec"

alias netstatosx="sudo lsof -i -n -P | grep TCP"


export GOPATH=$HOME/devel/_gopath
export PATH=/usr/local/mysql/bin:$HOME/bin:$GOPATH/bin:$PATH
# export DYLD_LIBRARY_PATH="/usr/local/mysql/lib:$DYLD_LIBRARY_PATH"
export EDITOR="e"
export PAGER="less"

export LC_CTYPE="en_US.UTF-8"
export LC_ALL=""

export HISTCONTROL=ignoredups:erasedups  # no duplicate entries
export HISTSIZE=100000                   # big big history
export HISTFILESIZE=100000               # big big history

HOSTNAME="$(hostname)"
HOSTNAME_SHORT="${HOSTNAME%%.*}"
HISTFILE="${HOME}/.history/$(date -u +%Y/%m/%d.%H.%M.%S)_${HOSTNAME_SHORT}_$$"

shopt -s histappend                      # append to history, don't overwrite it

function EXT_COLOR () { echo -ne "\e[38;5;$1m"; }
function CLOSE_COLOR () { echo -ne '\e[m'; }
export PS1="\[`EXT_COLOR 30`\]\h\[`CLOSE_COLOR`\]\[`EXT_COLOR 174`\] \w \$ \[`CLOSE_COLOR`\] > "
export LS_COLORS='di=38;5;108:fi=00:*svn-commit.tmp=31:ln=38;5;116:ex=38;5;186'
unset PROMPT_COMMAND

# Initialize RVM
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
