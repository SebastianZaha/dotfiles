#!/bin/sh

# Some aliases

alias ..='cd ..'
alias ...='cd ../../../'
alias ....='cd ../../../../'

alias grep="grep --color=auto"

alias ll="ls -l -h -a -G"

alias pg="ps ax | grep"

alias g="tar -czf"
alias ug="tar -xzf"
alias z="zip -r"
alias uz="unzip"

alias e="emacsclient --no-wait"
alias mcb="mc -b"

alias gst="git status"
alias gco="git commit -a -m"
alias gpu="git push"

alias qlf='qlmanage -p "$@" >& /dev/null'

alias b="bundle exec"

# Variables
export PATH=/Applications/Emacs.app/Contents/MacOS/bin:/opt/local/bin:/opt/local/sbin:/usr/local/mysql/bin:$HOME/bin:$HOME/.rvm/bin:$PATH
# export DYLD_LIBRARY_PATH="/usr/local/mysql/lib:$DYLD_LIBRARY_PATH"
export EDITOR="emacsclient"

# Initialize RVM
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
