{ user, ... }:

let
	alacrittyConfigPath = ../../../alacritty;
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
}
