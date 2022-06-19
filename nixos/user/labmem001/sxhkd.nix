{ user, pkgs, ... }:

let
	volume-change = pkgs.writeShellScriptBin "volume-change" ''
		# Check if argument exists
		if [ $# -ne 1 ]; then
			echo "Usage: $0 <amount>"
			exit 1
		fi

		# Check if argument is a number
		if ! [ "$1" -eq "$1" ] 2> /dev/null; then
			echo "<amount> should be an integer."
		fi

		amount=$1

		current_level=$(${pkgs.alsa-utils}/bin/amixer -M get Master | grep -oE "[0-9]+%" | sed "s/%//" | head -n 1)

		new_level=$((current_level + amount))
		if [ "$new_level" -lt 0 ]; then
			new_level=0
		fi
		new_level_percentage=$new_level

		# Convert new level to a linear factor
		new_level=$(echo "scale=2;" "$new_level / 100;" | ${pkgs.bc}/bin/bc | sed "s/^\./0./")

		${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ "$new_level"

		echo "Changed volume to $new_level_percentage%."
	'';
in {

	home.packages = with pkgs; [
		volume-change
	];

	services.sxhkd = {
		enable = true;
		keybindings = {
			"super + p" = "rofi -show drun -display-drun 'launch'";
			"super + control + p" = "power-menu";
			"super + r" = "rofi -show run";
			"super + w ; {a,c}" = "rofi -show {window,windowcd -display-windowcd 'window'}";
			"super + s" = "rofi -show ssh -no-parse-known-hosts -disable-history";
			"super + c" = "rofi -show calc -no-show-match -no-sort";
			"super + e" = "rofi -show emoji -matching normal";
			"{F2,F3}" = "${pkgs.brightnessctl}/bin/brightnessctl set {10%-,10%+}";
			"super + y" = "copyq show";
			"super + x ; r" = "${pkgs.maim}/bin/maim --select | ${pkgs.xclip}/bin/xclip -selection clipboard -target image/png";
			"super + x ; w" = "${pkgs.xdotool}/bin/xdotool getactivewindow | ${pkgs.xe}/bin/xe ${pkgs.maim}/bin/maim --window | ${pkgs.xclip}/bin/xclip -selection clipboard -target image/png";
			"super + x ; f" = "${pkgs.maim}/bin/maim | ${pkgs.xclip}/bin/xclip -selection clipboard -target image/png";
			# Launch a new terminal
			"super + Return" = user.terminal;
			# Audio Keys
			"XF86Audio{Lower,Raise}Volume" = "${volume-change}/bin/volume-change {-,+}5";
			"XF86AudioMute" = "${pkgs.alsa-utils}/bin/amixer -M set Master toggle";
			"XF86Audio{Prev,Play,Next}" = "${pkgs.playerctl}/bin/playerctl --player playerctld {previous,play-pause,next}";
			# Music
			"super + m ; {p,t,n}" = "${pkgs.mpc_cli}/bin/mpc {prev,toggle,next}";
			"super + m ; {-,+}" = "${pkgs.mpc_cli}/bin/mpc volume {-,+}10";
			"super + m ; {b,f}" = "${pkgs.mpc_cli}/bin/mpc seek {-,+}10";
			"super + m ; {r,s,l}" = "${pkgs.mpc_cli}/bin/mpc {repeat,random,single}";
			# Reload hotkey daemon
			"super + shift + r ; h" = "pkill -usr1 -x sxhkd";
		};
	};
}
