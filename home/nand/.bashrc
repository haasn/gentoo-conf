# /etc/skel/.bashrc
#
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !


# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi

# bash completion
source /usr/share/bash-completion/bash_completion

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
#HISTCONTROL=$HISTCONTROL${HISTCONTROL+:}ignoredups
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend
shopt -s autocd

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Reload appropriate settings
source /etc/profile

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias diff='colordiff'

    alias grep='grep -i --color=auto'
    alias fgrep='fgrep -i --color=auto'
    alias egrep='egrep -i --color=auto'
fi

# Fix mail settings
export MAILDIR=~/.imap/
export MAIL=$MAILDIR

# Colorful darcs!
export DARCS_DO_COLOR_LINES=1

# Preserve darcs completion
# complete -F _darcs -o default da

# * should expand hidden files as well
shopt -s dotglob

# Fix TERM for 256 color mode
# export TERM="rxvt-unicode-256color"

# Custom PS1
source ~/.git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWUPSTREAM="auto"
PS1='$(__git_ps1 "\[\e[38;5;70m\]%s ")\[\e[1;32m\]\u@\h\[\e[1;34m\] \w λ\[\e[0m\] '

# Increase history size and auto-syncronize
HISTSIZE=100000
HISTFILE=~/.histfile
PROMPT_COMMAND="history -a"
export HISTSIZE HISTFILE PROMPT_COMMAND

export EDITOR=vim

# Add F# to PATH
# PATH=/home/nand/dev/mono/dist/bin:$PATH

# Vi mode :)
# set -o vi

source ~/.aliases
