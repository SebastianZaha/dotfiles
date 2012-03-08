# As I only use bash as a shell, I keep only one config file (this one). 
# Splitting into .bashrc, .profile, .bash_aliases and so on is confusing and useless for me.

# Some aliases
alias ll="ls -l -h -a -G"
alias grep="grep --color=auto"
alias em="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -nq"

alias gst="git status"
alias gco="git commit -a -m"
alias gpu="git push"

# Variables
export PATH=$HOME/bin:/opt/local/bin:/opt/local/sbin:$HOME/.rvm/bin:$PATH
export EDITOR="emacsclient"

# Initialize RVM
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" 
