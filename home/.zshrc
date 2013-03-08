###
# Almost all of the configuration here is that which
# zsh and cominstall have inserted programmatically.
# The top portion, however, are scripts grabbed from
# my zsh confuration directory. Example script files:
# - functions
# - envvar
# - keymap
# - aliases
# - misc
# - prompt
# - whatever I coome up with
CONFDIR=$HOME"/.zsh.d/"
CUSTOM=".custom"
function zsh_safe_source () { safe_source "$CONFDIR$1" }
function safe_source () {
    if [ -n "$1" ]; then
        local FILE="$1"
        if [ -f $1 ]; then
            source $1
        else
            echo $1" not found"
        fi
    else
        echo "Need to know what to source."
    fi
}
zsh_safe_source "envvar"
zsh_safe_source "ohmyzsh"
zsh_safe_source "functions"
zsh_safe_source "keymap"
zsh_safe_source "aliases"
zsh_safe_source "misc"
zsh_safe_source "prompt"
if [ -f $HOME"/"$CUSTOM ]; then
    source $HOME"/"$CUSTOM
fi

# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' completions 1
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' glob 1
zstyle ':completion:*' list-suffixes true
zstyle ':completion:*' max-errors 2
zstyle ':completion:*' menu select=long
zstyle ':completion:*' preserve-prefix '//[^/]##/'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' substitute 1
zstyle :compinstall filename $HOME'/.zshrc'

autoload -Uz compinit
compinit -u
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=100000000
SAVEHIST=10000
setopt appendhistory autocd notify
unsetopt beep nomatch
bindkey -e
# End of lines configured by zsh-newuser-install
