{ pkgs, ... }:

let
	power-menu = pkgs.writeShellScriptBin "power-menu" ''
		choices=(suspend lock hibernate reboot poweroff)

		declare -A actions
		actions[suspend]="systemctl suspend"
		actions[lock]="physlock"
		actions[hibernate]="systemctl hibernate"
		actions[reboot]="systemctl reboot"
		actions[poweroff]="systemctl poweroff"

		# Ask for confirmation for actions that are irreversible
		confirmations=(reboot poweroff)

		choice=$(echo -e "''${choices[@]}" | rofi -sep " " -dmenu -p "power menu" -l 5)

		for i in "''${confirmations[@]}"; do
			if [[ $i == $choice ]]; then
				confirmation=$(echo -e "no\nyes" | rofi -dmenu -p "are you sure you want to ''${choice}" -l 5)
				if [[ $confirmation != "yes" ]]; then
					exit
				fi
			fi
		done

		eval "''${actions[$choice]}"
	'';
in {
	services.physlock = {
		enable = true;
		allowAnyUser = true;
	};
	
	environment.systemPackages = with pkgs; [
		power-menu
	];
}
