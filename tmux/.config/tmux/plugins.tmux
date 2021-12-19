# [ Tmux Package Manager Config ]

# Auto install Tmux Plugin Manager (TPM)
if "test ! -d ~/.tmux/plugins/tpm" "run 'git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm && ~/.tmux/plugins/tpm/bin/install_plugins'"

# Tmux Plugin Manager (TPM)
set -g @plugin "tmux-plugins/tpm"

# Vim Tmux Navigator: Better Vim and Tmux navigation
# Should also be installed in Vim
set -g @plugin "christoomey/vim-tmux-navigator"

# Initialize TMUX Plugin Manager
run "~/.tmux/plugins/tpm/tpm"
