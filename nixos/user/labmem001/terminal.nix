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

	xdg.configFile.vimKittyNavigatorFile1 = with pkgs.vimPlugins; {
		source = "${vim-kitty-navigator}/pass_keys.py";
		target = "kitty/pass_keys.py";
	};

	xdg.configFile.vimKittyNavigatorFile2 = with pkgs.vimPlugins; {
		source = "${vim-kitty-navigator}/neighboring_window.py";
		target = "kitty/neighboring_window.py";
	};

	xdg.configFile.kittyTheme = {
		text = "include themes/${user.theme}.conf";
		target = "kitty/theme.conf";
	};
}
