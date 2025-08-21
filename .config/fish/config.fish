if test -d "$HOME/.local/bin"
   fish_add_path "$HOME/.local/bin"
end

if test -n "$XDG_SESSION_TYPE"
and test "$XDG_SESSION_TYPE" = "wayland"
    set MOZ_ENABLE_WAYLAND 1
    set QT_QPA_PLATFORM wayland
    set ANKI_WAYLAND 1
end

set -g fish_greeting
set EDITOR "emacsclient -nw -a \"emacs -nw\""
set GIT_EDITOR "emacsclient -nw -a \"emacs -nw\""
set GPG_TTY $(tty)
set TERM xterm-24bit
set LC_ALL en_US.UTF-8
set LANG en_US.UTF-8
set LANGUAGE en_US.UTF-8
set LSP_USE_PLISTS true

# Rust paths
fish_add_path "$HOME/.cargo/bin"
fish_add_path "$HOME/.rustup"

# Go paths
fish_add_path "/usr/local/go/bin:$PATH"
fish_add_path "$HOME/.go"

# Bun paths
set BUN_INSTALL "$HOME/.bun"
fish_add_path "$BUN_INSTALL/bin"

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
