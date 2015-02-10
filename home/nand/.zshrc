# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _match _correct _approximate _prefix
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]} r:|[._-]=** r:|=**'
zstyle ':completion:*' max-errors 2
zstyle :compinstall filename '/home/nand/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
setopt appendhistory autocd notify
unsetopt beep nomatch
bindkey -e
# End of lines configured by zsh-newuser-install

# My own configuration
setopt dotglob

export MAILDIR=~/.imap
export MAIL=$MAILDIR
export EDITOR=vim

# Colors!
export DARCS_DO_COLOR_LINES=1

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

source ~/.git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWUPSTREAM="auto"

setopt PROMPT_SUBST
PS1=$'$(__git_ps1 "%%{\e[38;5;70m%%}%s ")%{\e[1;32m%}%n@%m%{\e[1;34m%} %~ λ%{\e[0m%} '

source ~/.aliases

# Git tab completion is really slow otherwise
__git_files () {
    _wanted files expl 'local files' _files
}

# Fix special keys
bindkey "${terminfo[kpp]}" history-beginning-search-backward
bindkey "${terminfo[knp]}" history-beginning-search-forward
bindkey "${terminfo[khome]}" beginning-of-line
bindkey "${terminfo[kend]}" end-of-line
bindkey "${terminfo[kdch1]}" delete-char

# Make these a bit more like bash
bindkey "^U" backward-kill-line

autoload edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line
