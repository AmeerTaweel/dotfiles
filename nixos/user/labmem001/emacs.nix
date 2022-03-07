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

			doom-themes
		];
	};

	xdg.configFile.emacsConfiguration = {
		source = emacsConfigurationPath;
		target = "emacs";
		recursive = true;
	};
}
