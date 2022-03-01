{ pkgs, ... }:

let
	emacsConfigurationPath = ../../../emacs;
in {
	programs.emacs = {
		enable = true;
	};

	xdg.configFile.emacsConfiguration = {
		source = emacsConfigurationPath;
		target = "emacs";
		recursive = true;
	};
}
