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
			ivy-rich

			rainbow-delimiters

			which-key

			helpful
		];
	};

	xdg.configFile.emacsConfiguration = {
		source = emacsConfigurationPath;
		target = "emacs";
		recursive = true;
	};
}
