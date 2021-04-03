# Enable VI key binding by default
fish_vi_key_bindings

# Let Fish look at the bin folder at the home directory to find scripts
set -U fish_user_paths $fish_user_paths $HOME/bin

# Turn off the greeting message
set fish_greeting

# Autocomplete and Highlight Colors
set fish_color_normal brcyan
set fish_color_autosuggestion '#7d7d7d'
set fish_color_command brcyan
set fish_color_error '#ff6c6b'
set fish_color_param brcyan

# Set editor to VIM
set EDITOR "vim"
# Set visual editor to VSCode
set VISUAL "code"

# Aliases

## Open Tmux with UTF8 support
alias tmux='tmux -u'

## List entries with long format, display the sizes in human readable format, and show hidden entries.
alias ll='ls -Alh'

## Colorize grep output (good for log files)
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

## Confirm before overwriting something
alias cp="cp -i"
alias mv='mv -i'
alias rm='rm -i'
