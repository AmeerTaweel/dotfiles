##################################################
# Keybindings
##################################################

# Reload .tmux.conf on prefix r
bind r source-file ~/.tmux.conf \; display-message "~/.tmux.conf Reloaded."

##################################################
## Prefix
##################################################
# Setting the prefix from CTRL-b to CTRL-a
set -g prefix C-a
# Ensure that we can send CTRL-a to other apps
bind C-a send-prefix
# Free the original CTRL-b keybinding
unbind C-b
##################################################

##################################################
## Splitting
##################################################
# Unbind original screen splitting bindings
unbind '"'
unbind %
# Split screen but with the current path
bind - split-window -v -c "#{pane_current_path}"
bind / split-window -h -c "#{pane_current_path}"
# Split screen with default path
bind _ split-window -v
bind | split-window -h
##################################################

##################################################
## Copy Mode
##################################################
# Enter copy mode by <Prefix> v
bind v copy-mode

# Use Vim keybindings in copy mode
setw -g mode-keys vi

# Pressing v to begin selection as in Vim
bind -T copy-mode-vi v send-keys -X begin-selection

# Pressing CTRL+v to make box selection
bind -T copy-mode-vi C-v send-keys -X rectangle-toggle \; send -X begin-selection

# Pressing y to copy selection to system clipboard
# Must have xclip installed for this to work
bind -T copy-mode-vi y send-keys -X copy-pipe-and-cancel "xclip -i -f -selection primary | xclip -i -selection clipboard"

# Pressing <Prefix> P (Capital Letter) will paste the contents of the last buffer
bind P paste-buffer

# Press <Prefix> CTRL+p to choose buffer
bind C-p choose-buffer
##################################################

##################################################
## Sessions
##################################################
# Fuzzy navigate to session
bind C-j new-window -n fuzzy-switch-session "tmux-fzf-switch-session"
##################################################

##################################################
## Windows
##################################################
# Create a new window in the current path
bind C-c new-window -c "#{pane_current_path}"
##################################################

##################################################
## Panes
##################################################
# Restoring Clear Screen
# CTRL-l is now used for navigation
# Hitting the key after the prefix will restore the old function
bind C-l send-keys C-l

# Break a pane out to a new window
bind b break-pane -d

# Join a pane from another window to this window
# Input format: window_index[.pane_index]
# pane_index is optional
bind j command-prompt -p "Join pane: " "join-pane -s :'%%'"

# Easily swap a pane (targeted by pane number) with the current pane
# Input format: pane_index
bind C-s display-panes\; command-prompt -p "Swap with pane: " "swap-pane -t '%%'"

# Synchronize panes in the session toggle
bind S set-window-option synchronize-panes
##################################################
