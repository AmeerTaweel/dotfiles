##################################################
# Settings
##################################################

# Make tmux read bash colors
set -g default-terminal "screen-256color"
set -g terminal-overrides ",xterm-256color:Tc"

# Enable mouse support
set -g mouse on

# Start window indexing at 1
set -g base-index 1
# Start pane indexing at 1
setw -g pane-base-index 1

# Keep the window numbers consistent
set -g renumber-windows on

# Address Vim window switching delay
set -s escape-time 0

# Increase scroll-back buffer size
set -g history-limit 50000

# Messages are displayed for x milliseconds
set -g display-time 4000

# Focus events enabled for terminals that support them
set -g focus-events on

##################################################
# Panes
##################################################

# Pane border styles
set -g pane-border-style $PANE_BORDER_STYLE
set -g pane-active-border-style $PANE_ACTIVE_BORDER_STYLE

# Display pane numbers colors
set -g display-panes-colour $DISPLAY_PANES_COLOR
set -g display-panes-active-colour $DISPLAY_PANES_ACTIVE_COLOR

##################################################
# Windows
##################################################

# Enable activity alerts
set -g visual-activity on

##################################################
# Status Bar
##################################################

# Update status bar every x seconds
set -g status-interval 2

# Global status bar and message styles
set -g message-style $MSG_STYLE
set -g status-style $STATUS_STYLE

##################################################
## Left Status Bar
##################################################

set -g status-format[0] "#[align=left]"

# Display padded session name
set -ag status-format[0] "$SESSION_NAME_FORMAT#(tmux-session-name-padding #S)"

##################################################
## Top Center Status Bar
##################################################

# Set the formatting for active and inactive windows
set -g window-status-current-format "$WINDOW_STATUS_CURRENT_FORMAT #I #W "
set -g window-status-format "$WINDOW_STATUS_FORMAT #I #W "

# Display windows in the top center
set -ag status-format[0] "#[align=centre]#{W:#{E:window-status-format},#{E:window-status-current-format}}"

##################################################
## Right Status Bar
##################################################

set -ag status-format[0] "#[align=right]"

set -ag status-format[0] "#[default]#(tmux-session-name-complement-padding #S)"

# Show hint on prefix key press
set -ag status-format[0] "#{?client_prefix,$PREFIX_HIGHLIGHT_FORMAT ^A ,\
    }#[default]"

# Separator
set -ag status-format[0] " "

# Show zoom state
set -ag status-format[0] "#{?window_zoomed_flag,$ZOOMED_MODE_FORMAT 🔎 ZOOMED ,\
$NORMAL_MODE_FORMAT 🔥 NORMAL }#[default]"
