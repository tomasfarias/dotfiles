# Lines configured by zsh-newuser-install
HISTFILE=~/.zshhistory
HISTSIZE=5000
SAVEHIST=5000
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/tomasfarias/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Initialize zsh plugins
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.zsh/zsh-completions/zsh-completions.plugin.zsh
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.zsh/zsh-autoenv/autoenv.zsh
source ~/.zsh/sudo/sudo.plugin.zsh

# Set PATH so it includes user's private ~/.local/bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

export EDITOR="emacs -nw"
export GPG_TTY=$(tty)
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

# Rust paths
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.rustup:$PATH"

# Go paths
export PATH="/usr/local/go/bin:$PATH"
export GOPATH="$HOME/.go"

# PyENV settings
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"
eval "$(pyenv virtualenv-init -)"

# Functions
function startBwSession() {
    export BW_SESSION=$(bw unlock --raw)
}

# Aliases
alias ls="exa -l"
alias cat="bat"
alias gh-login="bw get item 'GitHub' | jq -r '.fields[1].value' | gh auth login --with-token"
alias config="git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"

# Startup starship prompt
eval "$(starship init zsh)"
