{ user, pkgs, ... }:

{
	services.sxhkd = {
		enable = true;
		keybindings = {
			"super + p" = "rofi -show drun -display-drun 'launch'";
			"super + control + p" = "power-menu";
			"super + r" = "rofi -show run";
			"super + w ; {c,a}" = "rofi -show {window,windowcd -display-windowcd 'window'}";
			"super + s" = "rofi -show ssh -no-parse-known-hosts -disable-history";
			"super + c" = "rofi -show calc -no-show-match -no-sort";
			"super + e" = "rofi -show emoji -matching normal";
			"XF86Audio{RaiseVolume,LowerVolume,Mute}" = "${pkgs.pamixer}/bin/pamixer {--increase 5,--decrease 5,--toggle-mute} --allow-boost";
			"{F2,F3}" = "${pkgs.brightnessctl}/bin/brightnessctl set {10%-,10%+}";
			# Reload hotkey daemon
			"super + shift + r ; h" = "pkill -usr1 -x sxhkd";
			"super + y" = "copyq show";
			"super + x ; r"  = "${pkgs.maim}/bin/maim --select | ${pkgs.xclip}/bin/xclip -selection clipboard -target image/png";
			"super + x ; w" = "${pkgs.xdotool}/bin/xdotool getactivewindow | ${pkgs.xe}/bin/xe ${pkgs.maim}/bin/maim --window | ${pkgs.xclip}/bin/xclip -selection clipboard -target image/png";
			"super + x ; f"  = "${pkgs.maim}/bin/maim | ${pkgs.xclip}/bin/xclip -selection clipboard -target image/png";
		};
	};
}
