# Custom completion settings
zstyle ':completion:*' completer _expand _complete _ignored _match _correct _approximate _prefix
zstyle ':completion:*' expand suffix
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]} r:|[._-]=** r:|=**'
zstyle ':completion:*' max-errors 1
zstyle ':completion:*' original true
zstyle ':completion:*' select-prompt ''
zstyle :compinstall filename '/home/nand/.zshrc'

setopt nomatch
autoload -Uz compinit
compinit

# History settings
HISTFILE=~/.histfile
HISTSIZE=1000000
SAVEHIST=1000000
setopt incappendhistory sharehistory

# Miscellaneous
setopt dotglob autocd nomatch notify
unsetopt beep alwayslastprompt
bindkey -e

# Custom aliases
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

source ~/.aliases

# Git prompt
source ~/.git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWUPSTREAM="auto"
setopt promptsubst

autoload -U colors && colors
local lb='%{$fg_no_bold[default]%}[%{$reset_color%}'
      rb='%{$fg_no_bold[default]%}]%{$reset_color%}'

PS1="$lb%{$fg_bold[yellow]%}%D{%H:%M}$rb"\
"$lb%{$fg_bold[green]%}%n@%m$rb"\
"$lb%{$fg_bold[blue]%}%~$rb"\
$'$(__git_ps1 "[%%{\e[38;5;70m%%}%s%%{$reset_color%%}]")'\
"%(?..$lb%{$fg_bold[red]%}%?$rb)"\
$'\n%{%(?.$fg[green].$fg[red])%}λ%{$reset_color%} '

PS2="... "

__git_files () {
    # git tab completion is really slow otherwise
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
bindkey '^[[Z' reverse-menu-complete

autoload edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

autoload -Uz copy-earlier-word
zle -N copy-earlier-word
bindkey '^[m' copy-earlier-word

# Don't blow up on URLs
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

# Less greedy word boundaries
WORDCHARS=${WORDCHARS/\/}

# Environment vars
export EDITOR=vim
export DARCS_DO_COLOR_LINES=1
export GCC_COLORS=1
export __GL_SYNC_DISPLAY_DEVICE="DP-0"
