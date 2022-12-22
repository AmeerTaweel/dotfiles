{ user, pkgs, ... }:

{
	services.sxhkd = {
		enable = true;
		# Add mod variable
		keybindings = with pkgs; {
			"super + r; p" = "rofi -show drun -display-drun 'launch'";
			# "super + p" = "rofi -show drun -display-drun 'launch'";
			"super + control + p" = "power-menu";
			"super + r; c" = "rofi -show run";
			"super + w ; {a,c}" = "rofi -show {window,windowcd -display-windowcd 'window'}";
			"super + s" = "rofi -show ssh -no-parse-known-hosts -disable-history";
			"super + c" = "rofi -show calc -no-show-match -no-sort";
			"super + e" = "rofi -show emoji -matching normal";
			"{F2,F3}" = "${brightnessctl}/bin/brightnessctl set {10%-,10%+}";
			"super + y" = "copyq show";
			"super + x ; r" = "${maim}/bin/maim --select | ${xclip}/bin/xclip -selection clipboard -target image/png";
			"super + x ; w" = "${xdotool}/bin/xdotool getactivewindow | ${xe}/bin/xe ${maim}/bin/maim --window | ${xclip}/bin/xclip -selection clipboard -target image/png";
			"super + x ; f" = "${maim}/bin/maim | ${xclip}/bin/xclip -selection clipboard -target image/png";
			# Launch a new terminal
			"super + Return" = user.terminal;
			# Audio Keys
			"XF86Audio{Lower,Raise}Volume" = "${pamixer}/bin/pamixer --allow-boost {--decrease,--increase} 5";
			"XF86AudioMute" = "${pamixer}/bin/pamixer --allow-boost --toggle-mute";
			"XF86Audio{Prev,Play,Next}" = "${playerctl}/bin/playerctl --player playerctld {previous,play-pause,next}";
			# Music
			"super + m ; {p,t,n}" = "${mpc_cli}/bin/mpc {prev,toggle,next}";
			"super + m ; {-,+}" = "${mpc_cli}/bin/mpc volume {-,+}10";
			"super + m ; {b,f}" = "${mpc_cli}/bin/mpc seek {-,+}10";
			"super + m ; {r,s,l}" = "${mpc_cli}/bin/mpc {repeat,random,single}";
			# Reload hotkey daemon
			# "super + shift + r ; h" = "pkill -usr1 -x sxhkd";
		};
	};
}
