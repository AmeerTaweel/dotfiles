#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

# Possible power options
choices=()

# Command executed when an option is chosen
actions=()

# Should confirm before executing action?
confirm=()

choices+=("Suspend")
actions+=("systemctl suspend")
confirm+=(false)

choices+=("Lock")
actions+=("physlock")
confirm+=(false)

choices+=("Hibernate")
actions+=("systemctl hibernate")
confirm+=(false)

choices+=("Reboot")
actions+=("systemctl reboot")
confirm+=(true)

choices+=("PowerOff")
actions+=("systemctl poweroff")
confirm+=(true)

# Ask user for choice
choice=$(echo -e "${choices[@]}" | rofi -sep " " -dmenu -p "Power Menu" -l 5 -i)

for (( i = 0; i < ${#choices[@]}; i++ )); do
	if [[ "${choices[i]}" == "${choice}" ]]; then
		if [[ ${confirm[i]} == true ]]; then
			respns=$(echo -e "No\nYes" | rofi -dmenu -p "Are You Sure You Want To ${choice}?" -l 5 -i)
			if [[ $respns != "Yes" ]]; then
				exit
			fi
		fi
		eval "${actions[i]}"
	fi
done
