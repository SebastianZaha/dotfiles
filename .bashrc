#!/bin/bash

case "$(uname -s)" in
	Darwin)
		alias ls="ls -G"
		# -l use a long listing format
		alias ll="ls -l --human-readable --all -G"
		alias netstatosx="sudo lsof -i -n -P | grep TCP"
		;;
	Linux)
		alias ls='ls --color=auto'
		# -l use a long listing format
		alias ll="ls -l --human-readable --all --color=auto"
		alias mkpkg="makepkg --syncdeps --install --clean && git clean -dfx"
		;;
esac

alias vim='nvim'

alias ..='cd ..'
alias ...='cd ../../'
alias ....='cd ../../../'

alias pg="ps ax | grep"
alias grep="grep --color=auto"

alias b="bundle exec"
alias g="git"

export  GOBIN=$HOME/.local/bin
export GOPATH=$HOME/.local/gopath
export PATH=$PATH:$HOME/.local/bin
if [[ ! -v TERMUX_VERSION ]]; then
    export PATH=$PATH:/usr/local/go/bin:usr/local/mysql/bin:$HOME/devel/_android/platform-tools
fi
export EDITOR="nvim"

[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
[ -x /usr/bin/lesspipe.sh ] && eval "$(SHELL=/bin/sh lesspipe.sh)"
export PAGER="less"

export LC_CTYPE="en_US.UTF-8"
export LC_ALL=""

#########################################################################
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

unset PROMPT_COMMAND
export PS1='\[\033[01;31m\]\u\[\033[01;33m\]@\[\033[01;32m\]\h \[\033[01;33m\]\w \[\033[01;35m\]\$ \[\033[00m\]'

# Initialize RVM
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

export RIPGREP_CONFIG_PATH=~/.ripgrep

# .gitignored local configuration
[ -f ~/.bashrc_local ] && source ~/.bashrc_local

# Arch .bashrc_local example with fixes
#
# export _JAVA_AWT_WM_NONREPARENTING=1
# export QT_SCALE_FACTOR=2
# export GDK_SCALE=2
# [ "$(tty)" = "/dev/tty1" ] && exec startx


# only for interactive shells
if [[ $- == *i* ]]; then
	# disable Ctrl-s to 'lock' the tty 
	stty -ixon

	# disable history expansion, it just messes up my usage of ! in other commands, 
	# and I never use it (or know how to)
	set +H 

	[ -f ~/.fzf.bash ] && source ~/.fzf.bash
fi
