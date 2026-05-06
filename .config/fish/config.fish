if test -d "$HOME/.local/bin"
    fish_add_path "$HOME/.local/bin"
end

if test -n "$XDG_SESSION_TYPE"
    and test "$XDG_SESSION_TYPE" = wayland
    set MOZ_ENABLE_WAYLAND 1
    set QT_QPA_PLATFORM wayland
    set ANKI_WAYLAND 1
end

set -g fish_greeting
set -x -U EDITOR hx
set -x -U GIT_EDITOR hx
set GPG_TTY $(tty)
set -x -U TERM xterm-ghostty
set LC_ALL en_US.UTF-8
set LANG en_US.UTF-8
set LANGUAGE en_US.UTF-8
set LSP_USE_PLISTS true

# Rust paths
fish_add_path "$HOME/.cargo/bin"
fish_add_path "$HOME/.rustup"

# Go paths
set -x -U GOPATH "$HOME/.local/share/go"
set -g GOPATH "$HOME/.local/share/go"
fish_add_path /usr/local/go/bin
fish_add_path "$GOPATH/bin"

# Bun paths
set BUN_INSTALL "$HOME/.bun"
fish_add_path "$BUN_INSTALL/bin"

# Binds
bind super-e execute 'emacsclient -nw'
bind super-z execute zellij

# DOOM
fish_add_path "$HOME/.config/emacs/bin"
set -x -U DOOMDIR "$HOME/.config/doom"

# Aliases
function ls
    command eza -l $argv
end

function cat
    command bat $argv
end

function config
    command git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME $argv
end

function task
    command go-task $argv
end

# Start prompt
if status is-interactive
    starship init fish | source
    fzf --fish | source
end

# Ensure SSH agent
fish_ssh_agent

# Mise
if status is-interactive
    mise activate fish | source
end
