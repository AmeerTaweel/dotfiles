##################################################
# Tmux Package Manager Config
##################################################

# Auto install TMUX Plugin Manager
if "test ! -d ~/.tmux/plugins/tpm" "run 'git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm && ~/.tmux/plugins/tpm/bin/install_plugins'"

set -g @plugin "tmux-plugins/tpm"

# Vim TMUX Navigator: Better vim and tmux navigation
# Also installed in Vim with Vim Plug
set -g @plugin "christoomey/vim-tmux-navigator"

# TMUX URLView: Quickly open any url on TMUX window
set -g @plugin "tmux-plugins/tmux-urlview"

# TMUX Open: TMUX key bindings for quick opening of a highlighted file or url
set -g @plugin "tmux-plugins/tmux-open"

# TMUX FPP: Quickly open any path on TMUX window in $EDITOR
set -g @plugin 'tmux-plugins/tmux-fpp'

# Initialize TMUX Plugin Manager
run "~/.tmux/plugins/tpm/tpm"

# TMUX Open Config

## Change default search engine to DuckDuckGo
set -g @open-S "https://duckduckgo.com/?q="
