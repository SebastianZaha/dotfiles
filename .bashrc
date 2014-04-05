#!/bin/sh

# Some aliases

alias ..='cd ..'
alias ...='cd ../../../'
alias ....='cd ../../../../'

alias ll="ls -l -h -a -G"

alias cutw="cut -d ' ' -f"

alias pg="ps ax | grep"
alias grep="grep --color=auto"

alias g="tar -czf"
alias ug="tar -xzf"
alias z="zip -r"
alias uz="unzip"

alias e="emacsclient --no-wait"
alias m="mine"
alias mcb="mc -b"

alias gst="git status"
alias gco="git commit -a -m"
alias gpu="git push"

alias qlf='qlmanage -p "$@" >& /dev/null'

alias b="bundle exec"

# Variables
export PATH=/Applications/Emacs.app/Contents/MacOS/bin:/opt/local/bin:/opt/local/sbin:/usr/local/mysql/bin:$HOME/bin:$PATH
# export DYLD_LIBRARY_PATH="/usr/local/mysql/lib:$DYLD_LIBRARY_PATH"
export EDITOR="mine"

export HISTCONTROL=ignoredups:erasedups  # no duplicate entries
export HISTSIZE=100000                   # big big history
export HISTFILESIZE=100000               # big big history
shopt -s histappend                      # append to history, don't overwrite it


function EXT_COLOR () { echo -ne "\e[38;5;$1m"; }
function CLOSE_COLOR () { echo -ne '\e[m'; }
export PS1="\[`EXT_COLOR 187`\]\u@\h\[`CLOSE_COLOR`\]\[`EXT_COLOR 174`\] \w \$ \[`CLOSE_COLOR`\] > "
export LS_COLORS='di=38;5;108:fi=00:*svn-commit.tmp=31:ln=38;5;116:ex=38;5;186'

# Initialize RVM
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
