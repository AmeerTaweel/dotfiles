{ user, pkgs, ... }:

let
	rofiThemesPath = ../../../rofi/themes;
in {
	programs.rofi = {
		enable = true;
		cycle = true;
		terminal = user.terminal;
		theme = user.theme;
		plugins = with pkgs; [
			rofi-calc
			rofi-emoji
			rofi-file-browser
		];
		extraConfig = {
			modi = "window,windowcd,run,drun,ssh,calc,emoji,file-browser-extended";
			matching = "fuzzy";
			kb-cancel= "Escape";
		};
	};

	home.packages = with pkgs; [
		rofi-power-menu
	];

	xdg.configFile.rofiThemes = {
		source = rofiThemesPath;
		target = "rofi/themes";
		recursive = true;
	};
}
