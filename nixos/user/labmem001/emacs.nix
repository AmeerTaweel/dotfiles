{ pkgs, ... }:

let
	emacsConfigurationPath = ../../../emacs;
in {
	programs.emacs = {
		enable = true;
		extraPackages = emacsPackages: with emacsPackages; [
			use-package
			diminish

			# Evil Mode
			evil
			evil-collection

			ivy
			counsel
			swiper
			ivy-rich

			rainbow-delimiters

			which-key

			helpful

			doom-themes

			# Common Lisp
			slime
		];
	};

	xdg.configFile.emacsConfiguration = {
		source = emacsConfigurationPath;
		target = "emacs";
		recursive = true;
	};
}
