if test -d "$HOME/.local/bin"
   fish_add_path "$HOME/.local/bin"
end

if [ $XDG_SESSION_TYPE = "wayland" ]
    set MOZ_ENABLE_WAYLAND 1
    set QT_QPA_PLATFORM wayland
    set ANKI_WAYLAND 1
end

set -g fish_greeting
set EDITOR "emacsclient -t -a \"emacs -nw\""
set GPG_TTY $(tty)

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
end

# Ensure SSH agent
fish_ssh_agent

# ASDF
if status is-interactive
    source /opt/asdf-vm/asdf.fish
end
