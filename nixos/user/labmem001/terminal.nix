{ user, ... }:

let
	alacrittyConfigurationPath = ../../../alacritty;
in {
	programs.alacritty.enable = true;

	xdg.configFile.alacrittyConfiguration = {
		source = alacrittyConfigurationPath;
		target = "alacritty";
		recursive = true;
	};

	xdg.configFile.alacrittyTheme = {
		text = "import:\n  - ~/.config/alacritty/themes/${user.theme}.yml";
		target = "alacritty/theme.yml";
	};
}
