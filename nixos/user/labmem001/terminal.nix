{ user, pkgs, ... }:

let
	alacrittyConfigPath = ../../../alacritty;
	kittyConfigPath = ../../../kitty;
in {
	programs.alacritty.enable = true;

	xdg.configFile.alacrittyConfig = {
		source = alacrittyConfigPath;
		target = "alacritty";
		recursive = true;
	};

	xdg.configFile.alacrittyTheme = {
		text = "import:\n  - ~/.config/alacritty/themes/${user.theme}.yml";
		target = "alacritty/theme.yml";
	};

	home.packages = [ pkgs.kitty ];

	xdg.configFile.kittyConfig = {
		source = kittyConfigPath;
		target = "kitty";
		recursive = true;
	};

	xdg.configFile.kittyTheme = {
		text = "include themes/${user.theme}.conf";
		target = "kitty/theme.conf";
	};
}
