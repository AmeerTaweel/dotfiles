# [ Keybindings ]

# Reload .tmux.conf
bind r source-file ~/.tmux.conf \; display-message "~/.tmux.conf Reloaded."

# Restoring Clear Screen
# CTRL-l is now used for navigation
# Hitting the key after the prefix will restore the old function
bind C-l send-keys C-l

# [[ Prefix ]]

# Setting the prefix from CTRL-b to CTRL-a
set -g prefix C-a

# Ensure that we can send CTRL-a to other apps by double CTRL-a
bind C-a send-prefix

# Free the original CTRL-b keybinding
unbind C-b

# [[ Splitting ]]

# Unbind original screen splitting bindings
unbind '"'
unbind %

# Split screen with the current path as root
bind - split-window -v -c "#{pane_current_path}"
bind / split-window -h -c "#{pane_current_path}"

# Split screen with default path as root
bind _ split-window -v
bind ? split-window -h

# [[ Copy Mode ]]

# Enter copy mode
bind v copy-mode

# Use Vim keybindings in copy mode
setw -g mode-keys vi

# Pressing v to begin selection as in Vim
bind -T copy-mode-vi v send-keys -X begin-selection

# Pressing CTRL-v to make box selection
bind -T copy-mode-vi C-v send-keys -X rectangle-toggle \; send -X begin-selection

# Pressing y to copy selection to system clipboard
# Must have xclip installed for this to work
bind -T copy-mode-vi y send-keys -X copy-pipe-and-cancel "xclip -i -f -selection primary | xclip -i -selection clipboard"

# Pressing <prefix> P (Capital Letter) will paste the contents of the last buffer
bind P paste-buffer

# Press <prefix> CTRL-p to choose buffer
bind C-p choose-buffer

# [[ Sessions ]]

# Fuzzy switch session
bind C-j run-shell "bash ~/.config/tmux/scripts/session-switch.sh"

# [[ Windows ]]

# Create a new window in the current path
bind C-c new-window -c "#{pane_current_path}"

# [[ Panes ]]

# Break a pane out to a new window
bind b break-pane -d

# Swap a pane (targeted by pane number) with the current pane
bind C-s display-panes\; command-prompt -p "Swap with pane: " "swap-pane -t '%%'"

# Synchronize panes in the session toggle
bind S set-window-option synchronize-panes
