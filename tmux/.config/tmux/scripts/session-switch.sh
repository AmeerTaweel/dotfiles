#!/bin/bash

: '
Tmux session switcher using Rofi.

Dependencies:
	+ tmux
	+ rofi
	+ xargs
'

session_list=$(tmux list-sessions | sed -E "s/:.*$//")

current_session=$(tmux display-message -p "#S")

other_sessions=$(echo "$session_list" | grep -v "^$current_session$")

# The session that the user chose to switch to
target=$(echo "$other_sessions" | rofi -dmenu -p "switch session")

# If the user did not choose any session to switch, exit gracefully
if [ $? -ne 0 ]; then
	exit 0
fi

# Switch to the target session
echo "$target" | xe tmux switch-client -t
