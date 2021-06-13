##################################################
# Tmux Package Manager Config
##################################################

# Auto install TMUX Plugin Manager
if "test ! -d ~/.tmux/plugins/tpm" "run 'git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm && ~/.tmux/plugins/tpm/bin/install_plugins'"

set -g @plugin 'tmux-plugins/tpm'

# Tmux Battery: Get Battery Status
# set -g @plugin 'tmux-plugins/tmux-battery'

# Tmux CPU: Get CPU, RAM, and GPU Status
# set -g @plugin 'tmux-plugins/tmux-cpu'

# Tmux Online Status
# set -g @plugin 'tmux-plugins/tmux-online-status'

# Tmux Prefrix Highlight: Indicate when the prefix is clicked
# set -g @plugin 'tmux-plugins/tmux-prefix-highlight'

# Vim Tmux Navigator: Better vim and tmux navigation
# Also installed in Vim with Vim Plug
set -g @plugin 'christoomey/vim-tmux-navigator'

# Initialize TMUX Plugin Manager
run '~/.tmux/plugins/tpm/tpm'
