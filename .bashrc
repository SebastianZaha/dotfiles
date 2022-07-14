#!/bin/bash

# Some aliases

alias ..='cd ..'
alias ...='cd ../../../'
alias ....='cd ../../../../'

alias ls="ls --color"
alias ll="ls -l -h -a -G"

alias cutw="cut -d ' ' -f"
alias dus="du -shx"

alias pg="ps ax | grep"
alias grep="grep --color=auto"

alias e="emacs -nw"
alias vi="nvim"
alias vim="nvim"

alias gst="git status"
alias gcm="git commit --all --verbose"
alias gp="git push"

alias b="bundle exec"

alias netstatosx="sudo lsof -i -n -P | grep TCP"

alias mkpkg="makepkg --syncdeps --install --clean && git clean -dfx"

export  GOBIN=$HOME/.local/bin
export GOPATH=$HOME/.local/gopath
if [[ ! -v TERMUX_VERSION ]]; then
    export PATH=/usr/local/go/bin:usr/local/mysql/bin:$HOME/.rvm/bin:$PATH
fi
export PATH=$HOME/.local/bin:$PATH
export EDITOR="vim"

[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
[ -x /usr/bin/lesspipe.sh ] && eval "$(SHELL=/bin/sh lesspipe.sh)"
export PAGER="less"

export LC_CTYPE="en_US.UTF-8"
export LC_ALL=""

#########################################################################
# Save 5,000 lines of history in memory
HISTSIZE=10000
# Save 2,000,000 lines of history to disk (will have to grep ~/.bash_history for full listing)
HISTFILESIZE=2000000
# Append to history instead of overwrite
shopt -s histappend
# Ignore redundant or space commands
HISTCONTROL=ignoreboth
# Ignore more
HISTIGNORE='ls:ll:ls -alh:pwd:clear:history:ps ax'
# Set time format
HISTTIMEFORMAT='%F %T '
# Multiple commands on one line show up as a single line
shopt -s cmdhist
# Append new history lines, clear the history list, re-read the history list, print prompt.
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"
#########################################################################

# disable Ctrl-s to 'lock' the tty
stty -ixon

# disable history expansion, it just messes up my usage of ! in other commands, 
# and I never use it (or know how to)
set +H 

function EXT_COLOR () { echo -ne "\e[38;5;$1m"; }
function CLOSE_COLOR () { echo -ne '\e[m'; }
export PS1="\[`EXT_COLOR 30`\]\h\[`CLOSE_COLOR`\]\[`EXT_COLOR 174`\] \w \$ \[`CLOSE_COLOR`\] > "
export LS_COLORS='di=38;5;108:fi=00:*svn-commit.tmp=31:ln=38;5;116:ex=38;5;186'
unset PROMPT_COMMAND

# Initialize RVM
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

export RIPGREP_CONFIG_PATH=~/.ripgrep

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

[ "$(tty)" = "/dev/tty1" ] && exec sway

