{ user, pkgs, ... }:

{
	services.sxhkd = {
		enable = true;
		keybindings = {
			"super + p" = "rofi -show drun -display-drun 'launch'";
			"super + r" = "rofi -show run";
			"super + w ; {c,a}" = "rofi -show {window,windowcd -display-windowcd 'window'}";
			"super + s" = "rofi -show ssh -no-parse-known-hosts -disable-history";
			"super + c" = "rofi -show calc -no-show-match -no-sort";
			"super + e" = "rofi -show emoji -matching normal";
			"XF86Audio{RaiseVolume,LowerVolume,Mute}" = "${pkgs.pamixer}/bin/pamixer {--increase 5,--decrease 5,--toggle-mute} --allow-boost";
			"{F2,F3}" = "${pkgs.brightnessctl}/bin/brightnessctl set {10%-,10%+}";
			# Reload hotkey daemon
			"super + shift + r ; h" = "pkill -usr1 -x sxhkd";
			"super + y" = "rofi -show drun -display-drun 'launch'";
		};
	};
}
