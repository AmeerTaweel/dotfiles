{ pkgs, ... }:

let
	emacsConfigurationPath = ../../../emacs;
in {
	programs.emacs = {
		enable = true;
		extraPackages = emacsPackages: with emacsPackages; [
			use-package
			diminish

			ivy
			counsel
			swiper

			rainbow-delimiters

			which-key
		];
	};

	xdg.configFile.emacsConfiguration = {
		source = emacsConfigurationPath;
		target = "emacs";
		recursive = true;
	};
}
